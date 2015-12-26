{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.Names (desugarImports) where

import Prelude ()
import Prelude.Compat

import Data.List (find, nub)
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer (MonadWriter(..), censor)
import Control.Monad.State.Lazy

import qualified Data.Map as M

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Errors
import Language.PureScript.Traversals
import Language.PureScript.Externs
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Imports
import Language.PureScript.Sugar.Names.Exports
import Language.PureScript.Linter.Imports

-- |
-- Replaces all local names with qualified names within a list of modules. The
-- modules should be topologically sorted beforehand.
--
desugarImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => [ExternsFile] -> [Module] -> m [Module]
desugarImports externs modules = do
  env <- silence $ foldM externsEnv primEnv externs
  modules' <- traverse updateExportRefs modules
  (modules'', env') <- foldM updateEnv ([], env) modules'
  traverse (renameInModule' env') modules''
  where
  silence :: m a -> m a
  silence = censor (const mempty)

  -- | Create an environment from a collection of externs files
  externsEnv :: Env -> ExternsFile -> m Env
  externsEnv env ExternsFile{..} = do
    let members = Exports{..}
        ss = internalModuleSourceSpan "<Externs>"
        env' = M.insert efModuleName (ss, nullImports, members) env
        fromEFImport (ExternsImport mn mt qmn) = (mn, [(Nothing, mt, qmn)])
    imps <- foldM (resolveModuleImport env') nullImports (map fromEFImport efImports)
    exps <- resolveExports env' efModuleName imps members efExports
    return $ M.insert efModuleName (ss, imps, exps) env
    where

    exportedTypes :: [((ProperName, [ProperName]), ModuleName)]
    exportedTypes = mapMaybe toExportedType efExports
      where
      toExportedType (TypeRef tyCon dctors) = Just ((tyCon, fromMaybe (mapMaybe forTyCon efDeclarations) dctors), efModuleName)
        where
        forTyCon :: ExternsDeclaration -> Maybe ProperName
        forTyCon (EDDataConstructor pn _ tNm _ _) | tNm == tyCon = Just pn
        forTyCon _ = Nothing
      toExportedType (PositionedDeclarationRef _ _ r) = toExportedType r
      toExportedType _ = Nothing
    exportedTypeClasses :: [(ProperName, ModuleName)]
    exportedTypeClasses = mapMaybe toExportedTypeClass efExports
      where
      toExportedTypeClass (TypeClassRef className) = Just (className, efModuleName)
      toExportedTypeClass (PositionedDeclarationRef _ _ r) = toExportedTypeClass r
      toExportedTypeClass _ = Nothing
    exportedValues :: [(Ident, ModuleName)]
    exportedValues = mapMaybe toExportedValue efExports
      where
      toExportedValue (ValueRef ident) = Just (ident, efModuleName)
      toExportedValue (PositionedDeclarationRef _ _ r) = toExportedValue r
      toExportedValue _ = Nothing

  updateEnv :: ([Module], Env) -> Module -> m ([Module], Env)
  updateEnv (ms, env) m@(Module ss _ mn _ refs) =
    case mn `M.lookup` env of
      Just m' -> throwError . errorMessage $ RedefinedModule mn [envModuleSourceSpan m', ss]
      Nothing -> do
        members <- findExportable m
        let env' = M.insert mn (ss, nullImports, members) env
        (m', imps) <- resolveImports env' m
        exps <- maybe (return members) (resolveExports env' mn imps members) refs
        return (m' : ms, M.insert mn (ss, imps, exps) env)

  renameInModule' :: Env -> Module -> m Module
  renameInModule' env m@(Module _ _ mn _ _) =
    warnAndRethrow (addHint (ErrorInModule mn)) $ do
      let (_, imps, exps) = fromMaybe (internalError "Module is missing in renameInModule'") $ M.lookup mn env
      (m', used) <- flip runStateT M.empty $ renameInModule env imps (elaborateExports exps m)
      findUnusedImports m env used
      return m'

-- |
-- Make all exports for a module explicit. This may still effect modules that
-- have an exports list, as it will also make all data constructor exports
-- explicit.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module ss coms mn decls refs) =
  Module ss coms mn decls $
    Just $ map (\(ctor, dctors) -> TypeRef ctor (Just dctors)) (my exportedTypes) ++
           map TypeClassRef (my exportedTypeClasses) ++
           map ValueRef (my exportedValues) ++
           maybe [] (filter isModuleRef) refs
  where
  -- Extracts a list of values from the exports and filters out any values that
  -- are re-exports from other modules.
  my :: (Exports -> [(a, ModuleName)]) -> [a]
  my f = fst `map` filter ((== mn) . snd) (f exps)

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadState UsedImports m) => Env -> Imports -> Module -> m Module
renameInModule env imports (Module ss coms mn decls exps) =
  Module ss coms mn <$> parU decls go <*> pure exps
  where

  (go, _, _, _, _) = everywhereWithContextOnValuesM (Nothing, []) updateDecl updateValue updateBinder updateCase defS

  updateDecl :: (Maybe SourceSpan, [Ident]) -> Declaration -> m ((Maybe SourceSpan, [Ident]), Declaration)
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
  updateDecl (pos, bound) (FixityDeclaration fx name alias) =
    (,) (pos, bound) <$> (FixityDeclaration fx name <$> traverse (`updateValueName` pos) alias)
  updateDecl s d = return (s, d)

  updateValue :: (Maybe SourceSpan, [Ident]) -> Expr -> m ((Maybe SourceSpan, [Ident]), Expr)
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
  updateValue s@(pos, _) (Constructor name) =
    (,) s <$> (Constructor <$> updateDataConstructorName name pos)
  updateValue s@(pos, _) (TypedValue check val ty) =
    (,) s <$> (TypedValue check val <$> updateTypesEverywhere pos ty)
  updateValue s v = return (s, v)

  updateBinder :: (Maybe SourceSpan, [Ident]) -> Binder -> m ((Maybe SourceSpan, [Ident]), Binder)
  updateBinder (_, bound) v@(PositionedBinder pos _ _) =
    return ((Just pos, bound), v)
  updateBinder s@(pos, _) (ConstructorBinder name b) =
    (,) s <$> (ConstructorBinder <$> updateDataConstructorName name pos <*> pure b)
  updateBinder s (TypedBinder t b) = do
    (s'@ (span', _), b') <- updateBinder s b
    t' <- updateTypesEverywhere span' t
    return (s', TypedBinder t' b')
  updateBinder s v =
    return (s, v)

  updateCase :: (Maybe SourceSpan, [Ident]) -> CaseAlternative ->  m ((Maybe SourceSpan, [Ident]), CaseAlternative)
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
    updateType (TypeConstructor name) = TypeConstructor <$> updateTypeName name pos
    updateType (ConstrainedType cs t) = ConstrainedType <$> updateConstraints pos cs <*> pure t
    updateType t = return t

  updateConstraints :: Maybe SourceSpan -> [Constraint] -> m [Constraint]
  updateConstraints pos = traverse (\(name, ts) -> (,) <$> updateClassName name pos <*> traverse (updateTypesEverywhere pos) ts)

  updateTypeName :: Qualified ProperName -> Maybe SourceSpan -> m (Qualified ProperName)
  updateTypeName = update UnknownType (importedTypes imports) (resolveType . exportedTypes) TypeName (("type " ++) . runProperName)

  updateDataConstructorName :: Qualified ProperName -> Maybe SourceSpan -> m (Qualified ProperName)
  updateDataConstructorName = update (flip UnknownDataConstructor Nothing) (importedDataConstructors imports) (resolveDctor . exportedTypes) DctorName (("data constructor " ++) . runProperName)

  updateClassName  :: Qualified ProperName -> Maybe SourceSpan -> m (Qualified ProperName)
  updateClassName = update UnknownTypeClass (importedTypeClasses imports) (resolve . exportedTypeClasses) ClassName (("class " ++) . runProperName)

  updateValueName  :: Qualified Ident -> Maybe SourceSpan -> m (Qualified Ident)
  updateValueName = update UnknownValue (importedValues imports) (resolve . exportedValues) IdentName (("value " ++) . runIdent)

  -- Used when performing an update to qualify values and classes with their
  -- module of original definition.
  resolve :: (Eq a) => [(a, ModuleName)] -> a -> Maybe (Qualified a)
  resolve as name = mkQualified name <$> name `lookup` as

  -- Used when performing an update to qualify types with their module of
  -- original definition.
  resolveType :: [((ProperName, [ProperName]), ModuleName)] -> ProperName -> Maybe (Qualified ProperName)
  resolveType tys name = mkQualified name . snd <$> find ((== name) . fst . fst) tys

  -- Used when performing an update to qualify data constructors with their
  -- module of original definition.
  resolveDctor :: [((ProperName, [ProperName]), ModuleName)] -> ProperName -> Maybe (Qualified ProperName)
  resolveDctor tys name = mkQualified name . snd <$> find (elem name . snd . fst) tys

  -- Update names so unqualified references become qualified, and locally
  -- qualified references are replaced with their canoncial qualified names
  -- (e.g. M.Map -> Data.Map.Map).
  update
    :: (Ord a, Show a)
    => (Qualified a -> SimpleErrorMessage)
    -> M.Map (Qualified a) [(Qualified a, ModuleName)]
    -> (Exports -> a -> Maybe (Qualified a))
    -> (Qualified a -> Name)
    -> (a -> String)
    -> Qualified a
    -> Maybe SourceSpan
    -> m (Qualified a)
  update unknown imps getE toName render qname@(Qualified mn' name) pos = positioned $
    case (M.lookup qname imps, mn') of

      -- We found the name in our imports, so we return the name for it,
      -- qualifying with the name of the module it was originally defined in
      -- rather than the module we're importing from, to handle the case of
      -- re-exports. If there are multiple options for the name to resolve to
      -- in scope, we throw an error.
      (Just options, _) -> do
        checkImportConflicts render options
        let (Qualified (Just mnNew) _, mnOrig) = head options
        modify $ \result -> M.insert mnNew (maybe [toName qname] (toName qname :) (mnNew `M.lookup` result)) result
        return $ Qualified (Just mnOrig) name

      -- If the name wasn't found in our imports but was qualified then we need
      -- to check whether it's a failed import from a "pseudo" module (created
      -- by qualified importing). If that's not the case, then we just need to
      -- check it refers to a symbol in another module.
      (Nothing, Just mn'') -> do
        modExports <- getExports env mn''
        maybe (throwError . errorMessage $ unknown qname) return (getE modExports name)

      -- If neither of the above cases are true then it's an undefined or
      -- unimported symbol.
      _ -> throwError . errorMessage $ unknown qname

    where
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

-- |
-- Replaces `ProperRef` export values with a `TypeRef` or `TypeClassRef`
-- depending on what is availble within the module. Warns when a `ProperRef`
-- desugars into a `TypeClassRef`.
--
updateExportRefs :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Module -> m Module
updateExportRefs (Module ss coms mn decls exps) =
  Module ss coms mn decls <$> traverse (traverse updateRef) exps
  where

  updateRef :: DeclarationRef -> m DeclarationRef
  updateRef (ProperRef name)
     | name `elem` classNames = do
        tell . errorMessage $ DeprecatedClassExport name
        return $ TypeClassRef name
       -- Fall through case here - assume it's a type if it's not a class.
       -- If it's a reference to something that doesn't actually exist it will
       -- be picked up elsewhere
     | otherwise = return $ TypeRef name (Just [])
  updateRef (PositionedDeclarationRef pos com ref) =
    warnWithPosition pos $ PositionedDeclarationRef pos com <$> updateRef ref
  updateRef other = return other

  classNames :: [ProperName]
  classNames = mapMaybe go decls
    where
    go (PositionedDeclaration _ _ d) = go d
    go (TypeClassDeclaration name _ _ _) = Just name
    go _ = Nothing
