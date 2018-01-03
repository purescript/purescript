module Language.PureScript.Sugar.Names
  ( desugarImports
  , desugarImportsWithEnv
  , Env
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
import Control.Monad.Writer (MonadWriter(..), censor)

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
  => [ExternsFile]
  -> [Module]
  -> m [Module]
desugarImports externs modules =
  fmap snd (desugarImportsWithEnv externs modules)

desugarImportsWithEnv
  :: forall m
  . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [ExternsFile]
  -> [Module]
  -> m (Env, [Module])
desugarImportsWithEnv externs modules = do
  env <- silence $ foldM externsEnv primEnv externs
  (modules', env') <- first reverse <$> foldM updateEnv ([], env) modules
  (env',) <$> traverse (renameInModule' env') modules'
  where
  silence :: m a -> m a
  silence = censor (const mempty)

  -- | Create an environment from a collection of externs files
  externsEnv :: Env -> ExternsFile -> m Env
  externsEnv env ExternsFile{..} = do
    let members = Exports{..}
        env' = M.insert efModuleName (efSourceSpan, nullImports, members) env
        fromEFImport (ExternsImport mn mt qmn) = (mn, [(efSourceSpan, Just mt, qmn)])
    imps <- foldM (resolveModuleImport env') nullImports (map fromEFImport efImports)
    exps <- resolveExports env' efSourceSpan efModuleName imps members efExports
    return $ M.insert efModuleName (efSourceSpan, imps, exps) env
    where

    exportedTypes :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ModuleName)
    exportedTypes = M.fromList $ mapMaybe toExportedType efExports
      where
      toExportedType (TypeRef _ tyCon dctors) = Just (tyCon, (fromMaybe (mapMaybe forTyCon efDeclarations) dctors, efModuleName))
        where
        forTyCon :: ExternsDeclaration -> Maybe (ProperName 'ConstructorName)
        forTyCon (EDDataConstructor pn _ tNm _ _) | tNm == tyCon = Just pn
        forTyCon _ = Nothing
      toExportedType _ = Nothing

    exportedTypeOps :: M.Map (OpName 'TypeOpName) ModuleName
    exportedTypeOps = exportedRefs getTypeOpRef

    exportedTypeClasses :: M.Map (ProperName 'ClassName) ModuleName
    exportedTypeClasses = exportedRefs getTypeClassRef

    exportedValues :: M.Map Ident ModuleName
    exportedValues = exportedRefs getValueRef

    exportedValueOps :: M.Map (OpName 'ValueOpName) ModuleName
    exportedValueOps = exportedRefs getValueOpRef

    exportedRefs :: Ord a => (DeclarationRef -> Maybe a) -> M.Map a ModuleName
    exportedRefs f = M.fromList $ (, efModuleName) <$> mapMaybe f efExports

    exportedKinds :: M.Map (ProperName 'KindName) ModuleName
    exportedKinds = exportedRefs getKindRef

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
    flip map (M.toList (exportedTypes exps)) $ \(tctor, (dctors, mn')) ->
      let ref = TypeRef ss tctor (Just dctors)
      in if mn == mn' then ref else ReExportRef ss mn' ref

  go :: (a -> DeclarationRef) -> (Exports -> M.Map a ModuleName) -> [DeclarationRef]
  go toRef select =
    flip map (M.toList (select exps)) $ \(export, mn') ->
      if mn == mn' then toRef export else ReExportRef ss mn' (toRef export)

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
  updateDecl bound (DataDeclaration sa@(ss, _) dtype name args dctors) =
    fmap (bound,) $
      DataDeclaration sa dtype name
        <$> updateTypeArguments ss args
        <*> traverse (sndM (traverse (updateTypesEverywhere ss))) dctors
  updateDecl bound (TypeSynonymDeclaration sa@(ss, _) name ps ty) =
    fmap (bound,) $
      TypeSynonymDeclaration sa name
        <$> updateTypeArguments ss ps
        <*> updateTypesEverywhere ss ty
  updateDecl bound (TypeClassDeclaration sa@(ss, _) className args implies deps ds) =
    fmap (bound,) $
      TypeClassDeclaration sa className
        <$> updateTypeArguments ss args
        <*> updateConstraints ss implies
        <*> pure deps
        <*> pure ds
  updateDecl bound (TypeInstanceDeclaration sa@(ss, _) ch idx name cs cn ts ds) =
    fmap (bound,) $
      TypeInstanceDeclaration sa ch idx name
        <$> updateConstraints ss cs
        <*> updateClassName cn ss
        <*> traverse (updateTypesEverywhere ss) ts
        <*> pure ds
  updateDecl bound (TypeDeclaration (TypeDeclarationData sa@(ss, _) name ty)) =
    fmap (bound,) $
      TypeDeclaration . TypeDeclarationData sa name
        <$> updateTypesEverywhere ss ty
  updateDecl bound (ExternDeclaration sa@(ss, _) name ty) =
    fmap (name : bound,) $
      ExternDeclaration sa name
        <$> updateTypesEverywhere ss ty
  updateDecl bound (ExternDataDeclaration sa@(ss, _) name ki) =
    fmap (bound,) $
      ExternDataDeclaration sa name
        <$> updateKindsEverywhere ss ki
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
  updateValue (pos, bound) (Abs (VarBinder arg) val') =
    return ((pos, arg : bound), Abs (VarBinder arg) val')
  updateValue (pos, bound) (Let ds val') = do
    let args = mapMaybe letBoundVariable ds
    unless (length (ordNub args) == length args) .
      throwError . errorMessage' pos $ OverlappingNamesInLet
    return ((pos, args ++ bound), Let ds val')
  updateValue (pos, bound) (Var name'@(Qualified Nothing ident)) | ident `notElem` bound =
    (,) (pos, bound) <$> (Var <$> updateValueName name' pos)
  updateValue (pos, bound) (Var name'@(Qualified (Just _) _)) =
    (,) (pos, bound) <$> (Var <$> updateValueName name' pos)
  updateValue (pos, bound) (Op op) =
    (,) (pos, bound) <$> (Op <$> updateValueOpName op pos)
  updateValue s@(pos, _) (Constructor name) =
    (,) s <$> (Constructor <$> updateDataConstructorName name pos)
  updateValue s@(pos, _) (TypedValue check val ty) =
    (,) s <$> (TypedValue check val <$> updateTypesEverywhere pos ty)
  updateValue s v = return (s, v)

  updateBinder
    :: (SourceSpan, [Ident])
    -> Binder
    -> m ((SourceSpan, [Ident]), Binder)
  updateBinder (_, bound) v@(PositionedBinder pos _ _) =
    return ((pos, bound), v)
  updateBinder s@(pos, _) (ConstructorBinder name b) =
    (,) s <$> (ConstructorBinder <$> updateDataConstructorName name pos <*> pure b)
  updateBinder s@(pos, _) (OpBinder op) =
    (,) s <$> (OpBinder <$> updateValueOpName op pos)
  updateBinder s@(pos, _) (TypedBinder t b) = do
    t' <- updateTypesEverywhere pos t
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

  updateKindsEverywhere :: SourceSpan -> Kind -> m Kind
  updateKindsEverywhere pos = everywhereOnKindsM updateKind
    where
    updateKind :: Kind -> m Kind
    updateKind (NamedKind name) = NamedKind <$> updateKindName name pos
    updateKind k = return k

  updateTypeArguments
    :: (Traversable f, Traversable g)
    => SourceSpan
    -> f (a, g Kind) -> m (f (a, g Kind))
  updateTypeArguments pos = traverse (sndM (traverse (updateKindsEverywhere pos)))

  updateTypesEverywhere :: SourceSpan -> Type -> m Type
  updateTypesEverywhere pos = everywhereOnTypesM updateType
    where
    updateType :: Type -> m Type
    updateType (TypeOp name) = TypeOp <$> updateTypeOpName name pos
    updateType (TypeConstructor name) = TypeConstructor <$> updateTypeName name pos
    updateType (ConstrainedType c t) = ConstrainedType <$> updateInConstraint c <*> pure t
    updateType (KindedType t k) = KindedType t <$> updateKindsEverywhere pos k
    updateType t = return t
    updateInConstraint :: Constraint -> m Constraint
    updateInConstraint (Constraint name ts info) =
      Constraint <$> updateClassName name pos <*> pure ts <*> pure info

  updateConstraints :: SourceSpan -> [Constraint] -> m [Constraint]
  updateConstraints pos = traverse $ \(Constraint name ts info) ->
    Constraint
      <$> updateClassName name pos
      <*> traverse (updateTypesEverywhere pos) ts
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
        (mnNew, mnOrig) <- checkImportConflicts mn toName options
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
