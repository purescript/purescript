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
rename modules = trace (show $ findExports modules) $ mapM (renameInModule $ findExports modules) modules

renameInModule :: ExportEnvironment -> Module -> Either String Module
renameInModule exports m@(Module mn decls) = Module mn <$> mapM updateDecl decls >>= everywhereM (mkM updateType) >>= everywhereM (mkM updateValue) >>= everywhereM (mkM updateBinder)
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
    imports = resolveImports exports m
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

resolveImports :: ExportEnvironment -> Module -> ImportEnvironment
resolveImports env (Module mn decls) = ImportEnvironment (resolve scope $ exportedDataConstructors env)
                                                         (resolve scope $ exportedTypeClasses env)
    where
    scope = (mn : findImports decls)
    resolve imns = M.foldlWithKey (resolveDefs imns) M.empty
    resolveDefs imns result mn ids | mn `elem` imns = S.foldl (resolveDef mn) result ids
    resolveDefs imns result mn ids = result
    resolveDef mn result id = M.insert id (Qualified (Just mn) id) result
