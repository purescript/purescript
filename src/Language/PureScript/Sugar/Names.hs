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
import Control.Monad (foldM)
import Data.Generics (extM, mkM, everywhereM)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values
import Debug.Trace

data ExportEnvironment = ExportEnvironment
    { exportedDataConstructors :: M.Map ModuleName (S.Set ProperName)
    , exportedTypeClasses :: M.Map ModuleName (S.Set ProperName)
    } deriving (Show)
    
data ImportEnvironment = ImportEnvironment
    { importedDataConstructors :: M.Map ProperName (Qualified ProperName)
    , importedTypeClasses :: M.Map ProperName (Qualified ProperName)
    } deriving (Show)

nullEnv = ExportEnvironment M.empty M.empty

addDataConstructor :: ExportEnvironment -> ModuleName -> ProperName -> ExportEnvironment
addDataConstructor env mn id = env { exportedDataConstructors = M.insertWith S.union mn (S.singleton id) (exportedDataConstructors env) }

addTypeclass :: ExportEnvironment -> ModuleName -> ProperName -> ExportEnvironment
addTypeclass env mn id = env { exportedTypeClasses = M.insertWith S.union mn (S.singleton id) (exportedTypeClasses env) }

rename :: [Module] -> Either String [Module]
rename modules = mapM renameInModule' modules
    where
    exports = findExports modules
    renameInModule' m = do
        imports <- resolveImports exports m
        renameInModule imports m

renameInModule :: ImportEnvironment -> Module -> Either String Module
renameInModule imports (Module mn decls) =
    Module mn <$> mapM updateDecl decls >>= everywhereM (mkM updateType) >>= everywhereM (mkM updateValue) >>= everywhereM (mkM updateBinder)
    where
    updateDecl (TypeInstanceDeclaration cs (Qualified Nothing cn) ts ds) = do
      cn' <- updateClassName cn
      cs' <- updateConstraints cs
      return $ TypeInstanceDeclaration cs' cn' ts ds
    updateDecl d = return d
    updateValue (Constructor (Qualified Nothing nm)) = do
        nm' <- updateDataConstructorName nm
        return $ Constructor nm'
    updateValue v = return v
    updateBinder (ConstructorBinder (Qualified Nothing nm) b) = do
        nm' <- updateDataConstructorName nm
        return $ ConstructorBinder nm' b
    updateBinder v = return v
    updateType (ConstrainedType cs t) = do
      cs' <- updateConstraints cs
      return $ ConstrainedType cs' t
    updateType t = return t
    updateConstraints = mapM updateConstraint
    updateConstraint (Qualified Nothing nm, ts) = do
      nm' <- updateClassName nm
      return (nm', ts)
    updateConstraint other = return other
    updateClassName nm =
      maybe (err "typeclass" nm mn) return $ M.lookup nm (importedTypeClasses imports)
    updateDataConstructorName nm =
      maybe (err "data constructor" nm mn) return $ M.lookup nm (importedDataConstructors imports)
    err t nm mn = Left $ "Unknown " ++ t ++ " '" ++ show nm ++ "' in module '" ++ show mn ++ "'"

findExports :: [Module] -> ExportEnvironment
findExports = foldl addModule nullEnv
    where
    addModule env (Module mn ds) = foldl (addDecl mn) env ds
    addDecl mn env (TypeClassDeclaration tcn _ _) = addTypeclass env mn tcn
    addDecl mn env (DataDeclaration tcn _ dcs) = foldl (flip addDataConstructor mn) env (map fst dcs)
    addDecl _  env _ = env

findImports :: [Declaration] -> [ModuleName]
findImports decls = [ mn | (ImportDeclaration mn Nothing) <- decls ]

resolveImports :: ExportEnvironment -> Module -> Either String ImportEnvironment
resolveImports env (Module currentModule decls) = do
    dataConstructors <- resolve exportedDataConstructors
    typeClasses <- resolve exportedTypeClasses
    return $ ImportEnvironment dataConstructors typeClasses
    where
    scope = (currentModule : findImports decls)
    resolve get = foldM resolveDefs M.empty (M.toList $ get env)
    resolveDefs result (mn, names) | mn `elem` scope = foldM (resolveDef mn) result (S.toList names)
    resolveDefs result _ = return result
    resolveDef mn result name = case M.lookup name result of
        Nothing -> return $ M.insert name (Qualified (Just mn) name) result
        Just x@(Qualified (Just mn') _) -> if mn' == currentModule
            then Left $ "Module '" ++ show currentModule ++ "' defines '" ++ show name ++ "' which conflicts with imported definition '" ++ show (Qualified (Just mn) name) ++ "'"
            else Left $ "Module '" ++ show currentModule ++ "' has conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just mn) name) ++ "'"
