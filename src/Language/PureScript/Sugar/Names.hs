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
import Control.Arrow (first)
import Data.Maybe (fromJust)
import Data.Generics (mkM, everywhereM)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Extras
import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Pretty.Types
import Debug.Trace

data ExportEnvironment = ExportEnvironment { exportedTypeClasses :: M.Map ModuleName (S.Set ProperName) } deriving (Show)
data ImportEnvironment = ImportEnvironment { importedTypeClasses :: M.Map ProperName (Qualified ProperName) } deriving (Show)

nullEnv = ExportEnvironment M.empty

addTypeclass :: ExportEnvironment -> ModuleName -> ProperName -> ExportEnvironment
addTypeclass env mn id = env { exportedTypeClasses = M.insertWith S.union mn (S.singleton id) (exportedTypeClasses env) }

rename :: [Module] -> Either String [Module]
rename modules = mapM (renameInModule $ findExports modules) modules

renameInModule :: ExportEnvironment -> Module -> Either String Module
renameInModule exports m@(Module mn decls) = Module mn <$> everywhereM (mkM updateDecl) decls >>= everywhereM (mkM updateType)
    where
    updateDecl (TypeInstanceDeclaration cs cn ts ds) = do
      cn' <- updateClassName cn
      cs' <- updateConstraints cs
      return $ TypeInstanceDeclaration cs' cn' ts ds
    updateDecl d = return d
    updateType (ConstrainedType cs t) = do
      cs' <- updateConstraints cs
      return $ ConstrainedType cs' t
    updateType t = return t
    updateConstraints = mapM updateConstraint
    updateConstraint (nm, ts) = do
      nm' <- updateClassName nm
      return (nm', ts)
    updateClassName (Qualified Nothing nm) =
      maybe (Left $ "Unknown typeclass '" ++ show nm ++ "'") Right $ M.lookup nm (importedTypeClasses imports)
    updateClassName q = return q
    imports = resolveImports exports m

findExports :: [Module] -> ExportEnvironment
findExports = foldl addModule nullEnv
    where
    addModule env (Module mn ds) = foldl (addDecl mn) env ds
    addDecl mn env (TypeClassDeclaration tcn _ _) = addTypeclass env mn tcn
    addDecl _  env _ = env

findImports :: [Declaration] -> [ModuleName]
findImports decls = [ mn | (ImportDeclaration mn Nothing) <- decls ]

resolveImports :: ExportEnvironment -> Module -> ImportEnvironment
resolveImports env (Module mn decls) = ImportEnvironment $ resolve (mn : findImports decls) (exportedTypeClasses env)
    where
    resolve imns = M.foldlWithKey (resolveDefs imns) M.empty
    resolveDefs imns result mn ids | mn `elem` imns = S.foldl (resolveDef mn) result ids
    resolveDefs imns result mn ids = result
    resolveDef mn result id = M.insert id (Qualified (Just mn) id) result
