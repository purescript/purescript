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

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Lazy
import Control.Monad.Writer (MonadWriter(..), censor)

import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Externs
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
        ss = internalModuleSourceSpan "<Externs>"
        env' = M.insert efModuleName (ss, primImports, members) env
        fromEFImport (ExternsImport mn mt qmn) = (mn, [(Nothing, Just mt, qmn)])
    imps <- foldM (resolveModuleImport env') primImports (map fromEFImport efImports)
    exps <- resolveExports env' ss efModuleName imps members efExports
    return $ M.insert efModuleName (ss, imps, exps) env
    where

    exportedTypes :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ModuleName)
    exportedTypes = M.fromList $ mapMaybe toExportedType efExports
      where
      toExportedType (TypeRef tyCon dctors) = Just (tyCon, (fromMaybe (mapMaybe forTyCon efDeclarations) dctors, efModuleName))
        where
        forTyCon :: ExternsDeclaration -> Maybe (ProperName 'ConstructorName)
        forTyCon (EDDataConstructor pn _ tNm _ _) | tNm == tyCon = Just pn
        forTyCon _ = Nothing
      toExportedType (PositionedDeclarationRef _ _ r) = toExportedType r
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

  updateEnv :: ([Module], Env) -> Module -> m ([Module], Env)
  updateEnv (ms, env) m@(Module ss _ mn _ refs) =
    case mn `M.lookup` env of
      Just m' -> throwError . errorMessage $ RedefinedModule mn [envModuleSourceSpan m', ss]
      Nothing -> do
        members <- findExportable m
        let env' = M.insert mn (ss, primImports, members) env
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
-- Make all exports for a module explicit. This may still effect modules that
-- have an exports list, as it will also make all data constructor exports
-- explicit.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module ss coms mn decls refs) =
  Module ss coms mn decls $ Just
    $ elaboratedTypeRefs
    ++ go TypeOpRef exportedTypeOps
    ++ go TypeClassRef exportedTypeClasses
    ++ go ValueRef exportedValues
    ++ go ValueOpRef exportedValueOps
    ++ maybe [] (filter isModuleRef) refs
  where

  elaboratedTypeRefs :: [DeclarationRef]
  elaboratedTypeRefs =
    flip map (M.toList (exportedTypes exps)) $ \(tctor, (dctors, mn')) ->
      let ref = TypeRef tctor (Just dctors)
      in if mn == mn' then ref else ReExportRef mn' ref

  go :: (a -> DeclarationRef) -> (Exports -> M.Map a ModuleName) -> [DeclarationRef]
  go toRef select =
    flip map (M.toList (select exps)) $ \(export, mn') ->
      if mn == mn' then toRef export else ReExportRef mn' (toRef export)

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
renameInModule imports (Module ss coms mn decls exps) =
  Module ss coms mn <$> parU decls go <*> pure exps
  where

  (go, _, _, _, _) = everywhereWithContextOnValuesM (Nothing, []) updateDecl updateValue updateBinder updateCase defS

  updateDecl
    :: (Maybe SourceSpan, [Ident])
    -> Declaration
    -> m ((Maybe SourceSpan, [Ident]), Declaration)
  updateDecl (_, bound) d@(PositionedDeclaration pos _ _) =
    return ((Just pos, bound), d)
  updateDecl (pos, bound) (DataDeclaration dtype name args dctors) =
    (,) (pos, bound) <$> (DataDeclaration dtype name args <$> traverse (sndM (traverse (updateTypesEverywhere pos))) dctors)
  updateDecl (pos, bound) (TypeSynonymDeclaration name ps ty) =
    (,) (pos, bound) <$> (TypeSynonymDeclaration name ps <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (TypeClassDeclaration className args implies ds) =
    (,) (pos, bound) <$> (TypeClassDeclaration className args <$> updateConstraints pos implies <*> pure ds)
  updateDecl (pos, bound) (TypeInstanceDeclaration name cs cn ts ds) =
    (,) (pos, bound) <$> (TypeInstanceDeclaration name <$> updateConstraints pos cs <*> updateClassName cn pos <*> traverse (updateTypesEverywhere pos) ts <*> pure ds)
  updateDecl (pos, bound) (TypeDeclaration name ty) =
    (,) (pos, bound) <$> (TypeDeclaration name <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (ExternDeclaration name ty) =
    (,) (pos, name : bound) <$> (ExternDeclaration name <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (TypeFixityDeclaration fixity alias op) =
    (,) (pos, bound) <$> (TypeFixityDeclaration fixity <$> updateTypeName alias pos <*> pure op)
  updateDecl (pos, bound) (ValueFixityDeclaration fixity (Qualified mn' (Left alias)) op) =
    (,) (pos, bound) <$> (ValueFixityDeclaration fixity . fmap Left <$> updateValueName (Qualified mn' alias) pos <*> pure op)
  updateDecl (pos, bound) (ValueFixityDeclaration fixity (Qualified mn' (Right alias)) op) =
    (,) (pos, bound) <$> (ValueFixityDeclaration fixity . fmap Right <$> updateDataConstructorName (Qualified mn' alias) pos  <*> pure op)
  updateDecl s d = return (s, d)

  updateValue
    :: (Maybe SourceSpan, [Ident])
    -> Expr
    -> m ((Maybe SourceSpan, [Ident]), Expr)
  updateValue (_, bound) v@(PositionedValue pos' _ _) =
    return ((Just pos', bound), v)
  updateValue (pos, bound) (Abs (Left arg) val') =
    return ((pos, arg : bound), Abs (Left arg) val')
  updateValue (pos, bound) (Let ds val') = do
    let args = mapMaybe letBoundVariable ds
    unless (length (nub args) == length args) $
      maybe id rethrowWithPosition pos $
        throwError . errorMessage $ OverlappingNamesInLet
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
    :: (Maybe SourceSpan, [Ident])
    -> Binder
    -> m ((Maybe SourceSpan, [Ident]), Binder)
  updateBinder (_, bound) v@(PositionedBinder pos _ _) =
    return ((Just pos, bound), v)
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
    :: (Maybe SourceSpan, [Ident])
    -> CaseAlternative
    -> m ((Maybe SourceSpan, [Ident]), CaseAlternative)
  updateCase (pos, bound) c@(CaseAlternative bs _) =
    return ((pos, concatMap binderNames bs ++ bound), c)

  letBoundVariable :: Declaration -> Maybe Ident
  letBoundVariable (ValueDeclaration ident _ _ _) = Just ident
  letBoundVariable (PositionedDeclaration _ _ d) = letBoundVariable d
  letBoundVariable _ = Nothing

  updateTypesEverywhere :: Maybe SourceSpan -> Type -> m Type
  updateTypesEverywhere pos = everywhereOnTypesM updateType
    where
    updateType :: Type -> m Type
    updateType (TypeOp name) = TypeOp <$> updateTypeOpName name pos
    updateType (TypeConstructor name) = TypeConstructor <$> updateTypeName name pos
    updateType (ConstrainedType cs t) = ConstrainedType <$> traverse updateInConstraint cs <*> pure t
    updateType t = return t
    updateInConstraint :: Constraint -> m Constraint
    updateInConstraint (Constraint name ts info) =
      Constraint <$> updateClassName name pos <*> pure ts <*> pure info

  updateConstraints :: Maybe SourceSpan -> [Constraint] -> m [Constraint]
  updateConstraints pos = traverse $ \(Constraint name ts info) ->
    Constraint
      <$> updateClassName name pos
      <*> traverse (updateTypesEverywhere pos) ts
      <*> pure info

  updateTypeName
    :: Qualified (ProperName 'TypeName)
    -> Maybe SourceSpan
    -> m (Qualified (ProperName 'TypeName))
  updateTypeName = update (importedTypes imports) TyName

  updateTypeOpName
    :: Qualified (OpName 'TypeOpName)
    -> Maybe SourceSpan
    -> m (Qualified (OpName 'TypeOpName))
  updateTypeOpName = update (importedTypeOps imports) TyOpName

  updateDataConstructorName
    :: Qualified (ProperName 'ConstructorName)
    -> Maybe SourceSpan
    -> m (Qualified (ProperName 'ConstructorName))
  updateDataConstructorName = update (importedDataConstructors imports) DctorName

  updateClassName
    :: Qualified (ProperName 'ClassName)
    -> Maybe SourceSpan
    -> m (Qualified (ProperName 'ClassName))
  updateClassName = update (importedTypeClasses imports) TyClassName

  updateValueName :: Qualified Ident -> Maybe SourceSpan -> m (Qualified Ident)
  updateValueName = update (importedValues imports) IdentName

  updateValueOpName
    :: Qualified (OpName 'ValueOpName)
    -> Maybe SourceSpan
    -> m (Qualified (OpName 'ValueOpName))
  updateValueOpName = update (importedValueOps imports) ValOpName

  -- Update names so unqualified references become qualified, and locally
  -- qualified references are replaced with their canoncial qualified names
  -- (e.g. M.Map -> Data.Map.Map).
  update
    :: (Ord a)
    => M.Map (Qualified a) [ImportRecord a]
    -> (a -> Name)
    -> Qualified a
    -> Maybe SourceSpan
    -> m (Qualified a)
  update imps toName qname@(Qualified mn' name) pos = positioned $
    case (M.lookup qname imps, mn') of

      -- We found the name in our imports, so we return the name for it,
      -- qualifying with the name of the module it was originally defined in
      -- rather than the module we're importing from, to handle the case of
      -- re-exports. If there are multiple options for the name to resolve to
      -- in scope, we throw an error.
      (Just options, _) -> do
        (mnNew, mnOrig) <- checkImportConflicts mn toName options
        modify $ \result ->
          M.insert
            mnNew
            (maybe [fmap toName qname] (fmap toName qname :) (mnNew `M.lookup` result))
            result
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
    positioned err = maybe err (`warnAndRethrowWithPosition` err) pos
    throwUnknown = throwError . errorMessage . UnknownName . fmap toName $ qname
