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

import Data.Graph
import Data.List
import Data.Maybe (mapMaybe)

import Language.PureScript.CoreFn
import Language.PureScript.Names

-- |
-- Eliminate all declarations which are not a transitive dependency of the entry point module
--
eliminateDeadCode :: [ModuleName] -> [Module a] -> [Module a]
eliminateDeadCode entryPoints ms = map go ms
  where
  go (Module mn imps exps foreigns ds) = Module mn imps exps' foreigns ds'
    where
    ds' = filter (isUsed mn graph vertexFor entryPointVertices) ds
    names = concatMap bindIdents ds'
    exps' = filter (`elem` names) exps
  declarations = concatMap declarationsByModule ms
  (graph, _, vertexFor) = graphFromEdges $ map (\(key, deps) -> (key, key, deps)) declarations
  entryPointVertices = mapMaybe (vertexFor . fst) . filter (\((mn, _), _) -> mn `elem` entryPoints) $ declarations

  bindIdents :: Bind a -> [Ident]
  bindIdents (NotRec name _) = [name]
  bindIdents (Rec names) = map fst names

type Key = (ModuleName, Ident)

declarationsByModule :: Module a -> [(Key, [Key])]
declarationsByModule (Module mn _ _ fs ds) =
  let fs' = map (\(name, _, _) -> ((mn, name), [])) fs
  in fs' ++ concatMap go ds
  where
  go :: Bind a -> [(Key, [Key])]
  go d@(NotRec name _) = [((mn, name), dependencies mn d)]
  go d@(Rec names') = map (\(name, _) -> ((mn, name), dependencies mn d)) names'

dependencies :: ModuleName -> Bind a -> [Key]
dependencies mn =
  let (f, _, _, _) = everythingOnValues (++) (const []) values (const []) (const [])
  in nub . f
  where
  values :: Expr a -> [Key]
  values (Var ident) = [qualify mn ident]
  values _ = []

isUsed :: ModuleName -> Graph -> (Key -> Maybe Vertex) -> [Vertex] -> Bind a -> Bool
isUsed mn graph vertexFor entryPointVertices (NotRec name _) =
  isUsedValue mn graph vertexFor entryPointVertices name
isUsed mn graph vertexFor entryPointVertices (Rec ds) =
  any (isUsedValue mn graph vertexFor entryPointVertices . fst) ds

isUsedValue :: ModuleName -> Graph -> (Key -> Maybe Vertex) -> [Vertex] -> Ident -> Bool
isUsedValue mn graph vertexFor entryPointVertices name =
  let Just v' = vertexFor (mn, name)
  in any (\v -> path graph v v') entryPointVertices
