-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.Names (desugarImports) where

import Data.List (find, nub)
import Data.Maybe (fromMaybe, mapMaybe)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>), (<*>))
#endif
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer (MonadWriter(..))

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

-- |
-- Replaces all local names with qualified names within a list of modules. The
-- modules should be topologically sorted beforehand.
--
desugarImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => [ExternsFile] -> [Module] -> m [Module]
desugarImports externs modules = do
  env <- foldM externsEnv primEnv externs
  env' <- foldM updateEnv env modules
  mapM (renameInModule' env') modules
  where
  -- | Create an environment from a collection of externs files
  externsEnv :: Env -> ExternsFile -> m Env
  externsEnv env ExternsFile{..} = do
    let members = Exports{..}
        ss = internalModuleSourceSpan "<Externs>"
        env' = M.insert efModuleName (ss, nullImports, members) env
        fromEFImport (ExternsImport mn mt qmn) = (mn, [(Nothing, mt, qmn)])
    imps <- foldM (resolveModuleImport efModuleName env') nullImports (map fromEFImport efImports)
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

  updateEnv :: Env -> Module -> m Env
  updateEnv env m@(Module ss _ mn _ refs) =
    case mn `M.lookup` env of
      Just m' -> throwError . errorMessage $ RedefinedModule mn [envModuleSourceSpan m', ss]
      Nothing -> do
        members <- findExportable m
        let env' = M.insert mn (ss, nullImports, members) env
        imps <- resolveImports env' m
        exps <- maybe (return members) (resolveExports env' mn imps members) refs
        return $ M.insert mn (ss, imps, exps) env

  renameInModule' :: Env -> Module -> m Module
  renameInModule' env m@(Module _ _ mn _ _) =
    rethrow (addHint (ErrorInModule mn)) $ do
      let (_, imps, exps) = fromMaybe (internalError "Module is missing in renameInModule'") $ M.lookup mn env
      elaborateImports imps <$> renameInModule env imps (elaborateExports exps m)

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
-- Add `import X ()` for any modules where there are only fully qualified references to members.
-- This ensures transitive instances are included when using a member from a module.
--
elaborateImports :: Imports -> Module -> Module
elaborateImports imps (Module ss coms mn decls exps) = Module ss coms mn decls' exps
  where
  decls' :: [Declaration]
  decls' =
    let (f, _, _, _, _) = everythingOnValues (++) (const []) fqValues (const []) (const []) (const [])
    in mkImport `map` nub (f `concatMap` decls) ++ decls
  fqValues :: Expr -> [ModuleName]
  fqValues (Var (Qualified (Just mn') _)) | mn' `notElem` importedModules imps = [mn']
  fqValues _ = []
  mkImport :: ModuleName -> Declaration
  mkImport mn' = ImportDeclaration mn' (Explicit []) Nothing

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule :: forall m. (Applicative m, MonadError MultipleErrors m) => Env -> Imports -> Module -> m Module
renameInModule env imports (Module ss coms mn decls exps) =
  Module ss coms mn <$> parU decls go <*> pure exps
  where
  (go, _, _, _, _) = everywhereWithContextOnValuesM (Nothing, []) updateDecl updateValue updateBinder updateCase defS

  updateDecl :: (Maybe SourceSpan, [Ident]) -> Declaration -> m ((Maybe SourceSpan, [Ident]), Declaration)
  updateDecl (_, bound) d@(PositionedDeclaration pos _ _) =
    return ((Just pos, bound), d)
  updateDecl (pos, bound) (DataDeclaration dtype name args dctors) =
    (,) (pos, bound) <$> (DataDeclaration dtype name args <$> mapM (sndM (mapM (updateTypesEverywhere pos))) dctors)
  updateDecl (pos, bound) (TypeSynonymDeclaration name ps ty) =
    (,) (pos, bound) <$> (TypeSynonymDeclaration name ps <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (TypeClassDeclaration className args implies ds) =
    (,) (pos, bound) <$> (TypeClassDeclaration className args <$> updateConstraints pos implies <*> pure ds)
  updateDecl (pos, bound) (TypeInstanceDeclaration name cs cn ts ds) =
    (,) (pos, bound) <$> (TypeInstanceDeclaration name <$> updateConstraints pos cs <*> updateClassName cn pos <*> mapM (updateTypesEverywhere pos) ts <*> pure ds)
  updateDecl (pos, bound) (TypeDeclaration name ty) =
    (,) (pos, bound) <$> (TypeDeclaration name <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (ExternDeclaration name ty) =
    (,) (pos, name : bound) <$> (ExternDeclaration name <$> updateTypesEverywhere pos ty)
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

  updateCase :: (Maybe SourceSpan, [Ident]) -> CaseAlternative -> m ((Maybe SourceSpan, [Ident]), CaseAlternative)
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
  updateConstraints pos = mapM (\(name, ts) -> (,) <$> updateClassName name pos <*> mapM (updateTypesEverywhere pos) ts)

  updateTypeName :: Qualified ProperName -> Maybe SourceSpan -> m (Qualified ProperName)
  updateTypeName = update UnknownType (importedTypes imports) (resolveType . exportedTypes)

  updateDataConstructorName :: Qualified ProperName -> Maybe SourceSpan -> m (Qualified ProperName)
  updateDataConstructorName = update (flip UnknownDataConstructor Nothing) (importedDataConstructors imports) (resolveDctor . exportedTypes)

  updateClassName  :: Qualified ProperName -> Maybe SourceSpan -> m (Qualified ProperName)
  updateClassName = update UnknownTypeClass (importedTypeClasses imports) (resolve . exportedTypeClasses)

  updateValueName  :: Qualified Ident -> Maybe SourceSpan -> m (Qualified Ident)
  updateValueName = update UnknownValue (importedValues imports) (resolve . exportedValues)

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
  update :: (Ord a) => (Qualified a -> SimpleErrorMessage)
                       -> M.Map (Qualified a) (Qualified a, ModuleName)
                       -> (Exports -> a -> Maybe (Qualified a))
                       -> Qualified a
                       -> Maybe SourceSpan
                       -> m (Qualified a)
  update unknown imps getE qname@(Qualified mn' name) pos = positioned $
    case (M.lookup qname imps, mn') of
      -- We found the name in our imports, so we return the name for it,
      -- qualifying with the name of the module it was originally defined in
      -- rather than the module we're importing from, to handle the case of
      -- re-exports.
      (Just (_, mnOrig), _) -> return $ Qualified (Just mnOrig) name
      -- If the name wasn't found in our imports but was qualified then we need
      -- to check whether it's a failed import from a "pseudo" module (created
      -- by qualified importing). If that's not the case, then we just need to
      -- check it refers to a symbol in another module.
      (Nothing, Just mn'') -> do
        when (isExplicitQualModule mn'') . throwError . errorMessage $ unknown qname
        modExports <- getExports mn''
        maybe (throwError . errorMessage $ unknown qname) return (getE modExports name)
      -- If neither of the above cases are true then it's an undefined or
      -- unimported symbol.
      _ -> throwError . errorMessage $ unknown qname
    where
    isExplicitQualModule :: ModuleName -> Bool
    isExplicitQualModule = flip elem $ mapMaybe (\(Qualified q _) -> q) (M.keys imps)
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

  -- Gets the exports for a module, or an error message if the module doesn't exist
  getExports :: ModuleName -> m Exports
  getExports mn' = maybe (throwError . errorMessage $ UnknownModule mn') (return . envModuleExports) $ M.lookup mn' env
