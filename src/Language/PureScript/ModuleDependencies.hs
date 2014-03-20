-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.ModuleDependencies
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Provides the ability to sort modules based on module dependencies
--
-----------------------------------------------------------------------------

module Language.PureScript.ModuleDependencies (
  sortModules,
  ModuleGraph
) where

import Data.Data
import Data.Graph
import Data.Generics
import Data.List (nub)

import Language.PureScript.Declarations
import Language.PureScript.Names

-- |
-- A list of modules with their dependencies
--
type ModuleGraph = [(ModuleName, [ModuleName])]

-- |
-- Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
--
sortModules :: [Module] -> Either String ([Module], ModuleGraph)
sortModules ms = do
  let verts = map (\m -> (m, getModuleName m, usedModules m)) ms
  ms' <- mapM toModule $ stronglyConnComp verts
  let moduleGraph = map (\(_, mn, deps) -> (mn, deps)) verts
  return (ms', moduleGraph)

-- |
-- Calculate a list of used modules based on explicit imports and qualified names
--
usedModules :: (Data d) => d -> [ModuleName]
usedModules = nub . everything (++) (mkQ [] qualifiedIdents `extQ` qualifiedProperNames `extQ` imports)
  where
  qualifiedIdents :: Qualified Ident -> [ModuleName]
  qualifiedIdents (Qualified (Just mn) _) = [mn]
  qualifiedIdents _ = []
  qualifiedProperNames :: Qualified ProperName -> [ModuleName]
  qualifiedProperNames (Qualified (Just mn) _) = [mn]
  qualifiedProperNames _ = []
  imports :: Declaration -> [ModuleName]
  imports (ImportDeclaration mn _ _) = [mn]
  imports _ = []

getModuleName :: Module -> ModuleName
getModuleName (Module mn _ _) = mn

-- |
-- Convert a strongly connected component of the module graph to a module
--
toModule :: SCC Module -> Either String Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC [m]) = return m
toModule (CyclicSCC ms) = Left $ "Cycle in module dependencies: " ++ show (map getModuleName ms)
