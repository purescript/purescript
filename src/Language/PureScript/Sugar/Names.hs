-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.Names (
  rename
) where

import Data.Maybe (fromMaybe)
import Data.Generics (extM, mkM, everywhereM)
import Data.List (intersect)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, unless, foldM, liftM)

import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values

-- |
-- The global export environment - every declaration exported from every module
--
data ExportEnvironment = ExportEnvironment
    -- |
    -- The types exported from each module
    --
    { exportedTypes :: M.Map ModuleName (S.Set ProperName)
    -- |
    -- The data constructors exported from each module, grouped by type constructor
    --
    , exportedDataConstructors :: M.Map ModuleName (S.Set (ProperName, [ProperName]))
    -- |
    -- The classes exported from each module
    --
    , exportedTypeClasses :: M.Map ModuleName (S.Set ProperName)
    } deriving (Show)

-- |
-- An imported environment for a particular module. This also contains the module's own members.
--
data ImportEnvironment = ImportEnvironment
    -- |
    -- Local names for types within a module mapped to to their qualified names
    --
    { importedTypes :: M.Map ProperName (Qualified ProperName)
    -- |
    -- Local names for data constructors within a module mapped to to their qualified names
    --
    , importedDataConstructors :: M.Map ProperName (Qualified ProperName)
    -- |
    -- Local names for classes within a module mapped to to their qualified names
    --
    , importedTypeClasses :: M.Map ProperName (Qualified ProperName)
    } deriving (Show)

-- |
-- Adds a type belonging to a module to the export environment.
--
addType :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addType env mn name = do
    types <- addExport (exportedTypes env) mn name
    return $ env { exportedTypes = types  }

-- |
-- Adds a group of data constructors with their associated type constructor the export environment.
--
addDataConstructors :: ExportEnvironment -> ModuleName -> ProperName -> [ProperName] -> Either String ExportEnvironment
addDataConstructors env mn tcon dcons = do
    dataConstructors <- addExport (exportedDataConstructors env) mn (tcon, dcons)
    return $ env { exportedDataConstructors = dataConstructors }

-- |
-- Adds a class to the export environment.
--
addTypeclass :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addTypeclass env mn name = do
    classes <- addExport (exportedTypeClasses env) mn name
    return $ env { exportedTypeClasses = classes }

-- |
-- Adds an export to a map of exports of that type.
--
addExport :: (Ord s, Show s) => M.Map ModuleName (S.Set s) -> ModuleName -> s -> Either String (M.Map ModuleName (S.Set s))
addExport exports mn name = case M.lookup mn exports of
    Just s -> if S.member name s
              then Left $ "Module '" ++ show mn ++ "' has multiple definitions for '" ++ show name ++ "'"
              else Right $ M.insert mn (S.insert name s) exports
    Nothing -> Right $ M.insert mn (S.singleton name) exports

-- |
-- Replaces all local names with qualified names within a set of modules.
--
rename :: [Module] -> Either String [Module]
rename modules = do
    exports <- findExports modules
    mapM (renameInModule' exports) modules
    where
    renameInModule' exports m = do
        imports <- resolveImports exports m
        renameInModule imports m

-- |
-- Replaces all local names with qualified names within a module.
--
renameInModule :: ImportEnvironment -> Module -> Either String Module
renameInModule imports (Module mn decls) =
    Module mn <$> mapM updateDecl decls >>= everywhereM (mkM updateType `extM` updateValue `extM` updateBinder)
    where
    updateDecl (TypeInstanceDeclaration cs (Qualified Nothing cn) ts ds) = do
        cn' <- updateClassName cn
        cs' <- updateConstraints cs
        return $ TypeInstanceDeclaration cs' cn' ts ds
    updateDecl d = return d
    updateValue (Constructor (Qualified Nothing nm)) = liftM Constructor $ updateDataConstructorName nm
    updateValue v = return v
    updateBinder (ConstructorBinder (Qualified Nothing nm) b) = liftM (`ConstructorBinder` b) $ updateDataConstructorName nm
    updateBinder v = return v
    updateType (TypeConstructor (Qualified Nothing nm)) = liftM TypeConstructor $ updateTypeName nm
    updateType (SaturatedTypeSynonym (Qualified Nothing nm) tys) = do
        nm' <- updateTypeName nm
        tys' <- mapM updateType tys
        return $ SaturatedTypeSynonym nm' tys'
    updateType (ConstrainedType cs t) = liftM (`ConstrainedType` t) $ updateConstraints cs
    updateType t = return t
    updateConstraints = mapM updateConstraint
    updateConstraint (Qualified Nothing nm, ts) = do
        nm' <- updateClassName nm
        return (nm', ts)
    updateConstraint other = return other
    updateTypeName = update "type" importedTypes
    updateClassName = update "typeclass" importedTypeClasses
    updateDataConstructorName = update "data constructor" importedDataConstructors
    update t get nm = maybe (Left $ "Unknown " ++ t ++ " '" ++ show nm ++ "' in module '" ++ show mn ++ "'") return $ M.lookup nm (get imports)

-- |
-- Finds all exported declarations in a set of modules.
--
findExports :: [Module] -> Either String ExportEnvironment
findExports = foldM addModule (ExportEnvironment M.empty M.empty M.empty)
    where
    addModule env (Module mn ds) = foldM (addDecl mn) env ds
    addDecl mn env (TypeClassDeclaration tcn _ _) = addTypeclass env mn tcn
    addDecl mn env (DataDeclaration tn _ dcs) = do
        env' <- addDataConstructors env mn tn (map fst dcs)
        addType env' mn tn
    addDecl mn env (TypeSynonymDeclaration tn _ _) = addType env mn tn
    addDecl mn env (ExternDataDeclaration tn _) = addType env mn tn
    addDecl _  env _ = return env

-- |
-- Type representing a set of declarations being explicitly imported from a module
--
type ExplicitImports = [ImportType]

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports :: [Declaration] -> M.Map ModuleName (Maybe ExplicitImports)
findImports = foldl findImports' M.empty
    where
    findImports' result (ImportDeclaration mn expl) = M.insert mn expl result
    findImports' result _ = result

-- |
-- Constructs a local environment for a module.
--
resolveImports :: ExportEnvironment -> Module -> Either String ImportEnvironment
resolveImports env (Module currentModule decls) = ImportEnvironment
    <$> resolve (exportedTypes env) (resolveDefs typeImports)
    <*> resolve (exportedDataConstructors env) resolveDcons
    <*> resolve (exportedTypeClasses env) (resolveDefs (const []))
    where
    -- A Map from module name to imports from that module, where Nothing indicates everything is to be imported
    scope :: M.Map ModuleName (Maybe ExplicitImports)
    scope = M.insert currentModule Nothing (findImports decls)

    -- Resolve all exported names in a set by applying a type-specific resolver function
    resolve :: M.Map ModuleName (S.Set id) ->
               (M.Map ProperName (Qualified ProperName) -> ModuleName -> S.Set id -> Maybe ExplicitImports -> Either String (M.Map ProperName (Qualified ProperName))) ->
               Either String (M.Map ProperName (Qualified ProperName))
    resolve m f = foldM (\m' (mn, maybeExpl) -> f m' mn (fromMaybe S.empty $ mn `M.lookup` m) maybeExpl) M.empty (M.toList scope)

    -- Resolve a single name by adding it to the Map with a specified ModuleName, checking first
    -- that the name does not currently exist in scope.
    resolveDef :: (Ord id, Show id) => ModuleName ->
                                       M.Map id (Qualified id) ->
                                       id ->
                                       Either String (M.Map id (Qualified id))
    resolveDef mn result name = case M.lookup name result of
        Nothing -> return $ M.insert name (Qualified (Just mn) name) result
        Just x@(Qualified (Just mn') _) -> Left $ "Module '" ++ show currentModule ++
            if mn' == currentModule || mn == currentModule
            then "' defines '" ++ show name ++ "' which conflicts with imported definition '" ++ show (Qualified (Just mn) name) ++ "'"
            else "' has conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just mn) name) ++ "'"

    -- Resolve a set of ProperName exports for a specific imported set of names
    -- This function is used to resolve both types and type classes, hence the filter function as
    -- its first argument
    resolveDefs :: (ExplicitImports -> [ProperName]) ->
                   M.Map ProperName (Qualified ProperName) ->
                   ModuleName ->
                   S.Set ProperName ->
                   Maybe ExplicitImports ->
                   Either String (M.Map ProperName (Qualified ProperName))
    resolveDefs filt result mn names maybeExpl = do
      names' <- case maybeExpl of
        Just expl -> do
          forM_ (filt expl) $ \name ->
            unless (name `S.member` names) $ Left $ show name ++ " is explictly imported but is not defined in module " ++ show mn
          return $ S.toList names `intersect` filt expl
        Nothing -> return $ S.toList names
      foldM (resolveDef mn) result names'

    -- Resolve a set of data constructors keyed by their type constructor
    resolveDcons :: M.Map ProperName (Qualified ProperName) ->
                    ModuleName ->
                    S.Set (ProperName, [ProperName]) ->
                    Maybe ExplicitImports ->
                    Either String (M.Map ProperName (Qualified ProperName))
    resolveDcons result mn tcons maybeExpl = foldM (foldM $ resolveDef mn) result $ map snd $
      case maybeExpl of
        Just expl -> filter ((`elem` typeImports expl) . fst) (S.toList tcons)
        Nothing -> S.toList tcons
