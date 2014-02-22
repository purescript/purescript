-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.DeadCodeElimination
-- Copyright   :  (c) 2014 Phil Freeman
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.DeadCodeElimination (
  eliminateDeadCode
) where

import Data.Data
import Data.List
import Data.Graph
import Data.Generics
import Data.Maybe (mapMaybe)

import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Declarations
import Language.PureScript.TypeChecker.Monad

-- |
-- Eliminate all declarations which are not a transitive dependency of the entry point module
--
eliminateDeadCode :: Environment -> [ModuleName] -> [Module] -> [Module]
eliminateDeadCode env entryPoints ms =
  let declarations = concatMap (declarationsByModule env) ms
      (graph, _, vertexFor) = graphFromEdges $ map (\(key, deps) -> (key, key, deps)) declarations
      entryPointVertices = mapMaybe (vertexFor . fst) . filter (\((mn, _), _) -> mn `elem` entryPoints) $ declarations
  in flip map ms $ \(Module moduleName ds) -> Module moduleName (filter (isUsed moduleName graph vertexFor entryPointVertices) ds)

type Key = (ModuleName, Either Ident ProperName)

declarationsByModule :: Environment -> Module -> [(Key, [Key])]
declarationsByModule env (Module moduleName ds) = concatMap go ds
  where
  go :: Declaration -> [(Key, [Key])]
  go d@(ValueDeclaration name _ _ _) = [((moduleName, Left name), dependencies env moduleName d)]
  go (DataDeclaration _ _ dctors) = map (\(name, _) -> ((moduleName, Right name), [])) dctors
  go (ExternDeclaration _ name _ _) = [((moduleName, Left name), [])]
  go d@(BindingGroupDeclaration names') = map (\(name, _) -> ((moduleName, Left name), dependencies env moduleName d)) names'
  go (DataBindingGroupDeclaration ds') = concatMap go ds'
  go _ = []

dependencies :: (Data d) => Environment -> ModuleName -> d -> [Key]
dependencies env moduleName = nub . everything (++) (mkQ [] values)
  where
  values :: Value -> [Key]
  values (Var ident) = let (mn, name) = canonicalize moduleName env ident in [(mn, Left name)]
  values (Constructor pn) = let (mn, name) = canonicalizeDataConstructor moduleName env pn in [(mn, Right name)]
  values _ = []

isUsed :: ModuleName -> Graph -> (Key -> Maybe Vertex) -> [Vertex] -> Declaration -> Bool
isUsed moduleName graph vertexFor entryPointVertices (ValueDeclaration name _ _ _) =
  let Just v' = vertexFor (moduleName, Left name)
  in any (\v -> path graph v v') entryPointVertices
isUsed moduleName graph vertexFor entryPointVertices (DataDeclaration _ _ dctors) =
  any (\(pn, _) -> let Just v' = vertexFor (moduleName, Right pn)
                   in any (\v -> path graph v v') entryPointVertices) dctors
isUsed moduleName graph vertexFor entryPointVertices (ExternDeclaration _ name _ _) =
  let Just v' = vertexFor (moduleName, Left name)
  in any (\v -> path graph v v') entryPointVertices
isUsed moduleName graph vertexFor entryPointVertices (BindingGroupDeclaration ds) =
  any (\(name, _) -> let Just v' = vertexFor (moduleName, Left name)
                     in any (\v -> path graph v v') entryPointVertices) ds
isUsed moduleName graph vertexFor entryPointVertices (DataBindingGroupDeclaration ds) =
  any (isUsed moduleName graph vertexFor entryPointVertices) ds
isUsed _ _ _ _ _ = True
