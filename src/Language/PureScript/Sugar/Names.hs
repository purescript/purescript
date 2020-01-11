module Language.PureScript.Sugar.Names
  ( desugarImports
  , desugarImportsWithEnv
  , Env
  , externsEnv
  , primEnv
  , ImportRecord(..)
  , ImportProvenance(..)
  , Imports(..)
  , Exports(..)
  ) where

import Prelude.Compat
import Protolude (ordNub, sortBy, on)

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Lazy
import Control.Monad.Writer (MonadWriter(..))

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Kinds
import Language.PureScript.Linter.Imports
import Language.PureScript.Names
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Exports
import Language.PureScript.Sugar.Names.Imports
import Language.PureScript.Traversals
import Language.PureScript.Types

-- |
-- Replaces all local names with qualified names within a list of modules. The
-- modules should be topologically sorted beforehand.
--
desugarImports
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Env
  -> [Module]
  -> m [Module]
desugarImports env modules =
  fmap snd (desugarImportsWithEnv env modules)

desugarImportsWithEnv
  :: forall m
  . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Env
  -> [Module]
  -> m (Env, [Module])
desugarImportsWithEnv e modules = do
  (modules', env') <- first reverse <$> foldM updateEnv ([], e) modules
  (env',) <$> traverse (renameInModule' env') modules'
  where
  updateEnv :: ([Module], Env) -> Module -> m ([Module], Env)
  updateEnv (ms, env) m@(Module ss _ mn _ refs) = do
    members <- findExportable m
    let env' = M.insert mn (ss, nullImports, members) env
    (m', imps) <- resolveImports env' m
    exps <- maybe (return members) (resolveExports env' ss mn imps members) refs
    return (m' : ms, M.insert mn (ss, imps, exps) env)

  renameInModule' :: Env -> Module -> m Module
  renameInModule' env m@(Module _ _ mn _ _) =
    warnAndRethrow (addHint (ErrorInModule mn)) $ do
      let (_, imps, exps) = fromMaybe (internalError "Module is missing in renameInModule'") $ M.lookup mn env
      (m', used) <- flip runStateT M.empty $ renameInModule imps m
      let m'' = elaborateExports exps m'
      lintImports m'' env used
      return m''

-- | Create an environment from a collection of externs files
externsEnv
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Env
  -> ExternsFile
  -> m Env
externsEnv env ExternsFile{..} = do
  let members = Exports{..}
      env' = M.insert efModuleName (efSourceSpan, nullImports, members) env
      fromEFImport (ExternsImport mn mt qmn) = (mn, [(efSourceSpan, Just mt, qmn)])
  imps <- foldM (resolveModuleImport env') nullImports (map fromEFImport efImports)
  exps <- resolveExports env' efSourceSpan efModuleName imps members efExports
  return $ M.insert efModuleName (efSourceSpan, imps, exps) env
  where

  -- An ExportSource for declarations local to the module which the given
  -- ExternsFile corresponds to.
  localExportSource =
    ExportSource { exportSourceDefinedIn = efModuleName
                  , exportSourceImportedFrom = Nothing
                  }

  exportedTypes :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
  exportedTypes = M.fromList $ mapMaybe toExportedType efExports
    where
    toExportedType (TypeRef _ tyCon dctors) = Just (tyCon, (fromMaybe (mapMaybe forTyCon efDeclarations) dctors, localExportSource))
      where
      forTyCon :: ExternsDeclaration -> Maybe (ProperName 'ConstructorName)
      forTyCon (EDDataConstructor pn _ tNm _ _) | tNm == tyCon = Just pn
      forTyCon _ = Nothing
    toExportedType _ = Nothing

  exportedTypeOps :: M.Map (OpName 'TypeOpName) ExportSource
  exportedTypeOps = exportedRefs getTypeOpRef

  exportedTypeClasses :: M.Map (ProperName 'ClassName) ExportSource
  exportedTypeClasses = exportedRefs getTypeClassRef

  exportedValues :: M.Map Ident ExportSource
  exportedValues = exportedRefs getValueRef

  exportedValueOps :: M.Map (OpName 'ValueOpName) ExportSource
  exportedValueOps = exportedRefs getValueOpRef

  exportedKinds :: M.Map (ProperName 'KindName) ExportSource
  exportedKinds = exportedRefs getKindRef

  exportedRefs :: Ord a => (DeclarationRef -> Maybe a) -> M.Map a ExportSource
  exportedRefs f =
    M.fromList $ (, localExportSource) <$> mapMaybe f efExports

-- |
-- Make all exports for a module explicit. This may still affect modules that
-- have an exports list, as it will also make all data constructor exports
-- explicit.
--
-- The exports will appear in the same order as they do in the existing exports
-- list, or if there is no export list, declarations are order based on their
-- order of appearance in the module.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module ss coms mn decls refs) =
  Module ss coms mn decls $ Just $ reorderExports decls refs
    $ elaboratedTypeRefs
    ++ go (TypeOpRef ss) exportedTypeOps
    ++ go (TypeClassRef ss) exportedTypeClasses
    ++ go (ValueRef ss) exportedValues
    ++ go (ValueOpRef ss) exportedValueOps
    ++ go (KindRef ss) exportedKinds
    ++ maybe [] (filter isModuleRef) refs
  where

  elaboratedTypeRefs :: [DeclarationRef]
  elaboratedTypeRefs =
    flip map (M.toList (exportedTypes exps)) $ \(tctor, (dctors, src)) ->
      let ref = TypeRef ss tctor (Just dctors)
      in if mn == exportSourceDefinedIn src then ref else ReExportRef ss src ref

  go :: (a -> DeclarationRef) -> (Exports -> M.Map a ExportSource) -> [DeclarationRef]
  go toRef select =
    flip map (M.toList (select exps)) $ \(export, src) ->
      if mn == exportSourceDefinedIn src then toRef export else ReExportRef ss src (toRef export)

-- |
-- Given a list of declarations, an original exports list, and an elaborated
-- exports list, reorder the elaborated list so that it matches the original
-- order. If there is no original exports list, reorder declarations based on
-- their order in the source file.
reorderExports :: [Declaration] -> Maybe [DeclarationRef] -> [DeclarationRef] -> [DeclarationRef]
reorderExports decls originalRefs =
  sortBy (compare `on` originalIndex)
  where
  names =
    maybe (mapMaybe declName decls) (map declRefName) originalRefs
  namesMap =
    M.fromList $ zip names [(0::Int)..]
  originalIndex ref =
    M.lookup (declRefName ref) namesMap

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadState UsedImports m)
  => Imports
  -> Module
  -> m Module
renameInModule imports (Module modSS coms mn decls exps) =
  Module modSS coms mn <$> parU decls go <*> pure exps
  where

  (go, _, _, _, _) =
    everywhereWithContextOnValuesM
      (modSS, [])
      (\(_, bound) d -> (\(bound', d') -> ((declSourceSpan d', bound'), d')) <$> updateDecl bound d)
      updateValue
      updateBinder
      updateCase
      defS

  updateDecl
    :: [Ident]
    -> Declaration
    -> m ([Ident], Declaration)
  updateDecl bound (DataDeclaration sa dtype name args dctors) =
    fmap (bound,) $
      DataDeclaration sa dtype name
        <$> updateTypeArguments args
        <*> traverse (traverseDataCtorFields (traverse (sndM updateTypesEverywhere))) dctors
  updateDecl bound (TypeSynonymDeclaration sa name ps ty) =
    fmap (bound,) $
      TypeSynonymDeclaration sa name
        <$> updateTypeArguments ps
        <*> updateTypesEverywhere ty
  updateDecl bound (TypeClassDeclaration sa@(ss, _) className args implies deps ds) =
    fmap (bound,) $
      TypeClassDeclaration sa className
        <$> updateTypeArguments args
        <*> updateConstraints ss implies
        <*> pure deps
        <*> pure ds
  updateDecl bound (TypeInstanceDeclaration sa@(ss, _) ch idx name cs cn ts ds) =
    fmap (bound,) $
      TypeInstanceDeclaration sa ch idx name
        <$> updateConstraints ss cs
        <*> updateClassName cn ss
        <*> traverse updateTypesEverywhere ts
        <*> pure ds
  updateDecl bound (TypeDeclaration (TypeDeclarationData sa name ty)) =
    fmap (bound,) $
      TypeDeclaration . TypeDeclarationData sa name
        <$> updateTypesEverywhere ty
  updateDecl bound (ExternDeclaration sa name ty) =
    fmap (name : bound,) $
      ExternDeclaration sa name
        <$> updateTypesEverywhere ty
  updateDecl bound (ExternDataDeclaration sa name ki) =
    fmap (bound,) $
      ExternDataDeclaration sa name
        <$> updateKindsEverywhere ki
  updateDecl bound (TypeFixityDeclaration sa@(ss, _) fixity alias op) =
    fmap (bound,) $
      TypeFixityDeclaration sa fixity
        <$> updateTypeName alias ss
        <*> pure op
  updateDecl bound (ValueFixityDeclaration sa@(ss, _) fixity (Qualified mn' (Left alias)) op) =
    fmap (bound,) $
      ValueFixityDeclaration sa fixity . fmap Left
        <$> updateValueName (Qualified mn' alias) ss
        <*> pure op
  updateDecl bound (ValueFixityDeclaration sa@(ss, _) fixity (Qualified mn' (Right alias)) op) =
    fmap (bound,) $
      ValueFixityDeclaration sa fixity . fmap Right
        <$> updateDataConstructorName (Qualified mn' alias) ss
        <*> pure op
  updateDecl b d =
    return (b, d)

  updateValue
    :: (SourceSpan, [Ident])
    -> Expr
    -> m ((SourceSpan, [Ident]), Expr)
  updateValue (_, bound) v@(PositionedValue pos' _ _) =
    return ((pos', bound), v)
  updateValue (pos, bound) (Abs (VarBinder ss arg) val') =
    return ((pos, arg : bound), Abs (VarBinder ss arg) val')
  updateValue (pos, bound) (Let w ds val') = do
    let args = mapMaybe letBoundVariable ds
    unless (length (ordNub args) == length args) .
      throwError . errorMessage' pos $ OverlappingNamesInLet
    return ((pos, args ++ bound), Let w ds val')
  updateValue (_, bound) (Var ss name'@(Qualified Nothing ident)) | ident `notElem` bound =
    (,) (ss, bound) <$> (Var ss <$> updateValueName name' ss)
  updateValue (_, bound) (Var ss name'@(Qualified (Just _) _)) =
    (,) (ss, bound) <$> (Var ss <$> updateValueName name' ss)
  updateValue (_, bound) (Op ss op) =
    (,) (ss, bound) <$> (Op ss <$> updateValueOpName op ss)
  updateValue (_, bound) (Constructor ss name) =
    (,) (ss, bound) <$> (Constructor ss <$> updateDataConstructorName name ss)
  updateValue s (TypedValue check val ty) =
    (,) s <$> (TypedValue check val <$> updateTypesEverywhere ty)
  updateValue s v = return (s, v)

  updateBinder
    :: (SourceSpan, [Ident])
    -> Binder
    -> m ((SourceSpan, [Ident]), Binder)
  updateBinder (_, bound) v@(PositionedBinder pos _ _) =
    return ((pos, bound), v)
  updateBinder (_, bound) (ConstructorBinder ss name b) =
    (,) (ss, bound) <$> (ConstructorBinder ss <$> updateDataConstructorName name ss <*> pure b)
  updateBinder (_, bound) (OpBinder ss op) =
    (,) (ss, bound) <$> (OpBinder ss <$> updateValueOpName op ss)
  updateBinder s (TypedBinder t b) = do
    t' <- updateTypesEverywhere t
    return (s, TypedBinder t' b)
  updateBinder s v =
    return (s, v)

  updateCase
    :: (SourceSpan, [Ident])
    -> CaseAlternative
    -> m ((SourceSpan, [Ident]), CaseAlternative)
  updateCase (pos, bound) c@(CaseAlternative bs gs) =
    return ((pos, concatMap binderNames bs ++ updateGuard gs ++ bound), c)
    where
    updateGuard :: [GuardedExpr] -> [Ident]
    updateGuard [] = []
    updateGuard (GuardedExpr g _ : xs) =
      concatMap updatePatGuard g ++ updateGuard xs
      where
        updatePatGuard (PatternGuard b _) = binderNames b
        updatePatGuard _                  = []

  letBoundVariable :: Declaration -> Maybe Ident
  letBoundVariable = fmap valdeclIdent . getValueDeclaration

  updateKindsEverywhere :: SourceKind -> m SourceKind
  updateKindsEverywhere = everywhereOnKindsM updateKind
    where
    updateKind :: SourceKind -> m SourceKind
    updateKind (NamedKind ann@(ss, _) name) = NamedKind ann <$> updateKindName name ss
    updateKind k = return k

  updateTypeArguments
    :: (Traversable f, Traversable g)
    => f (a, g SourceKind) -> m (f (a, g SourceKind))
  updateTypeArguments = traverse (sndM (traverse updateKindsEverywhere))

  updateTypesEverywhere :: SourceType -> m SourceType
  updateTypesEverywhere = everywhereOnTypesM updateType
    where
    updateType :: SourceType -> m SourceType
    updateType (TypeOp ann@(ss, _) name) = TypeOp ann <$> updateTypeOpName name ss
    updateType (TypeConstructor ann@(ss, _) name) = TypeConstructor ann <$> updateTypeName name ss
    updateType (ConstrainedType ann c t) = ConstrainedType ann <$> updateInConstraint c <*> pure t
    updateType (ForAll ann v mbK t sco) = case mbK of
      Nothing -> pure $ ForAll ann v Nothing t sco
      Just k -> ForAll ann v <$> fmap pure (updateKindsEverywhere k) <*> pure t <*> pure sco
    updateType (KindedType ann t k) = KindedType ann t <$> updateKindsEverywhere k
    updateType t = return t
    updateInConstraint :: SourceConstraint -> m SourceConstraint
    updateInConstraint (Constraint ann@(ss, _) name ts info) =
      Constraint ann <$> updateClassName name ss <*> pure ts <*> pure info

  updateConstraints :: SourceSpan -> [SourceConstraint] -> m [SourceConstraint]
  updateConstraints pos = traverse $ \(Constraint ann name ts info) ->
    Constraint ann
      <$> updateClassName name pos
      <*> traverse updateTypesEverywhere ts
      <*> pure info

  updateTypeName
    :: Qualified (ProperName 'TypeName)
    -> SourceSpan
    -> m (Qualified (ProperName 'TypeName))
  updateTypeName = update (importedTypes imports) TyName

  updateTypeOpName
    :: Qualified (OpName 'TypeOpName)
    -> SourceSpan
    -> m (Qualified (OpName 'TypeOpName))
  updateTypeOpName = update (importedTypeOps imports) TyOpName

  updateDataConstructorName
    :: Qualified (ProperName 'ConstructorName)
    -> SourceSpan
    -> m (Qualified (ProperName 'ConstructorName))
  updateDataConstructorName = update (importedDataConstructors imports) DctorName

  updateClassName
    :: Qualified (ProperName 'ClassName)
    -> SourceSpan
    -> m (Qualified (ProperName 'ClassName))
  updateClassName = update (importedTypeClasses imports) TyClassName

  updateValueName :: Qualified Ident -> SourceSpan -> m (Qualified Ident)
  updateValueName = update (importedValues imports) IdentName

  updateValueOpName
    :: Qualified (OpName 'ValueOpName)
    -> SourceSpan
    -> m (Qualified (OpName 'ValueOpName))
  updateValueOpName = update (importedValueOps imports) ValOpName

  updateKindName
    :: Qualified (ProperName 'KindName)
    -> SourceSpan
    -> m (Qualified (ProperName 'KindName))
  updateKindName = update (importedKinds imports) KiName

  -- Update names so unqualified references become qualified, and locally
  -- qualified references are replaced with their canoncial qualified names
  -- (e.g. M.Map -> Data.Map.Map).
  update
    :: (Ord a)
    => M.Map (Qualified a) [ImportRecord a]
    -> (a -> Name)
    -> Qualified a
    -> SourceSpan
    -> m (Qualified a)
  update imps toName qname@(Qualified mn' name) pos = warnAndRethrowWithPosition pos $
    case (M.lookup qname imps, mn') of

      -- We found the name in our imports, so we return the name for it,
      -- qualifying with the name of the module it was originally defined in
      -- rather than the module we're importing from, to handle the case of
      -- re-exports. If there are multiple options for the name to resolve to
      -- in scope, we throw an error.
      (Just options, _) -> do
        (mnNew, mnOrig) <- checkImportConflicts pos mn toName options
        modify $ \usedImports ->
          M.insertWith (++) mnNew [fmap toName qname] usedImports
        return $ Qualified (Just mnOrig) name

      -- If the name wasn't found in our imports but was qualified then we need
      -- to check whether it's a failed import from a "pseudo" module (created
      -- by qualified importing). If that's not the case, then we just need to
      -- check it refers to a symbol in another module.
      (Nothing, Just mn'') ->
        if mn'' `S.member` importedQualModules imports || mn'' `S.member` importedModules imports
        then throwUnknown
        else throwError . errorMessage . UnknownName . Qualified Nothing $ ModName mn''

      -- If neither of the above cases are true then it's an undefined or
      -- unimported symbol.
      _ -> throwUnknown

    where
    throwUnknown = throwError . errorMessage . UnknownName . fmap toName $ qname
