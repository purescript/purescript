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

import Data.Graph
import Data.List (nub)
import Data.Maybe (mapMaybe)

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Types

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
  let verts = map (\m@(Module _ ds _) -> (m, getModuleName m, nub (concatMap usedModules ds))) ms
  ms' <- mapM toModule $ stronglyConnComp verts
  let moduleGraph = map (\(_, mn, deps) -> (mn, deps)) verts
  return (ms', moduleGraph)

-- |
-- Calculate a list of used modules based on explicit imports and qualified names
--
usedModules :: Declaration -> [ModuleName]
usedModules = let (f, _, _, _, _) = everythingOnValues (++) forDecls forValues (const []) (const []) (const []) in nub . f
  where
  forDecls :: Declaration -> [ModuleName]
  forDecls (ImportDeclaration mn _ _) = [mn]
  forDecls _ = []

  forValues :: Expr -> [ModuleName]
  forValues (Var (Qualified (Just mn) _)) = [mn]
  forValues (Constructor (Qualified (Just mn) _)) = [mn]
  forValues (TypedValue _ _ ty) = forTypes ty
  forValues _ = []

  forTypes :: Type -> [ModuleName]
  forTypes (TypeConstructor (Qualified (Just mn) _)) = [mn]
  forTypes (ConstrainedType cs _) = mapMaybe (\(Qualified mn _, _) -> mn) cs
  forTypes _ = []

-- |
-- Convert a strongly connected component of the module graph to a module
--
toModule :: SCC Module -> Either String Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC [m]) = return m
toModule (CyclicSCC ms) = Left $ "Cycle in module dependencies: " ++ show (map getModuleName ms)
