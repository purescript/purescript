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

import Control.Applicative ((<$>))
import Control.Monad (foldM, liftM)
import Data.Generics (extM, mkM, everywhereM)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values

data ExportEnvironment = ExportEnvironment
    { exportedTypes :: M.Map ModuleName (S.Set ProperName)
    , exportedDataConstructors :: M.Map ModuleName (S.Set ProperName)
    , exportedTypeClasses :: M.Map ModuleName (S.Set ProperName)
    } deriving (Show)
    
data ImportEnvironment = ImportEnvironment
    { importedTypes :: M.Map ProperName (Qualified ProperName)
    , importedDataConstructors :: M.Map ProperName (Qualified ProperName)
    , importedTypeClasses :: M.Map ProperName (Qualified ProperName)
    } deriving (Show)

nullEnv = ExportEnvironment M.empty M.empty M.empty

addType :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addType env mn name = do
    types <- addExport (exportedTypes env) mn name
    return $ env { exportedTypes = types  }

addDataConstructor :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addDataConstructor env mn name = do
    dataConstructors <- addExport (exportedDataConstructors env) mn name
    return $ env { exportedDataConstructors = dataConstructors }

addTypeclass :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addTypeclass env mn name = do
    classes <- addExport (exportedTypeClasses env) mn name
    return $ env { exportedTypeClasses = classes }

addExport :: (Ord s, Show s) => M.Map ModuleName (S.Set s) -> ModuleName -> s -> Either String (M.Map ModuleName (S.Set s))
addExport exports mn name = case M.lookup mn exports of
    Just s -> if S.member name s
              then Left $ "Module '" ++ show mn ++ "' has multiple definitions for '" ++ (show name) ++ "'"
              else Right $ M.insert mn (S.insert name s) exports
    Nothing -> Right $ M.insert mn (S.singleton name) exports

rename :: [Module] -> Either String [Module]
rename modules = do
    exports <- findExports modules
    mapM (renameInModule' exports) modules
    where
    renameInModule' exports m = do
        imports <- resolveImports exports m
        renameInModule imports m

renameInModule :: ImportEnvironment -> Module -> Either String Module
renameInModule imports (Module mn decls) =
    Module mn <$> mapM updateDecl decls >>= everywhereM ((mkM updateType) `extM` updateValue `extM` updateBinder)
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
    
findExports :: [Module] -> Either String ExportEnvironment
findExports = foldM addModule nullEnv
    where
    addModule env (Module mn ds) = foldM (addDecl mn) env ds
    addDecl mn env (TypeClassDeclaration tcn _ _) = addTypeclass env mn tcn
    addDecl mn env (DataDeclaration tn _ dcs) = do
      env' <- foldM (`addDataConstructor` mn) env (map fst dcs)
      addType env' mn tn
    addDecl mn env (TypeSynonymDeclaration tn _ _) = addType env mn tn
    addDecl mn env (ExternDataDeclaration tn _) = addType env mn tn
    addDecl _  env _ = return env

findImports :: [Declaration] -> [ModuleName]
findImports decls = [ mn | (ImportDeclaration mn Nothing) <- decls ]

resolveImports :: ExportEnvironment -> Module -> Either String ImportEnvironment
resolveImports env (Module currentModule decls) = do
    types <- resolve exportedTypes
    dataConstructors <- resolve exportedDataConstructors
    typeClasses <- resolve exportedTypeClasses
    return $ ImportEnvironment types dataConstructors typeClasses
    where
    scope = currentModule : findImports decls
    resolve get = foldM resolveDefs M.empty (M.toList $ get env)
    resolveDefs result (mn, names) | mn `elem` scope = foldM (resolveDef mn) result (S.toList names)
    resolveDefs result _ = return result
    resolveDef mn result name = case M.lookup name result of
        Nothing -> return $ M.insert name (Qualified (Just mn) name) result
        Just x@(Qualified (Just mn') _) -> Left $ "Module '" ++ show currentModule ++ if mn' == currentModule
            then "' defines '" ++ show name ++ "' which conflicts with imported definition '" ++ show (Qualified (Just mn) name) ++ "'"
            else "' has conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just mn) name) ++ "'"
