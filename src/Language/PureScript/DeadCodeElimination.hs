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
-- | Dead code elimination.
--
-----------------------------------------------------------------------------

module Language.PureScript.DeadCodeElimination (
  eliminateDeadCode
) where

import Data.Graph
import Data.List
import Data.Maybe (mapMaybe)

import Language.PureScript.Core
import Language.PureScript.CoreFn
import Language.PureScript.Names

-- |
-- Eliminate all declarations which are not a transitive dependency of the entry point module
--
eliminateDeadCode :: [ModuleName] -> [Module (Bind a)] -> [Module (Bind a)]
eliminateDeadCode entryPoints ms = map go ms
  where
  go (Module coms mn imps exps foreigns ds) = Module coms mn imps exps' foreigns' ds'
    where
    ds' = filter (isUsed mn graph vertexFor entryPointVertices) ds
    foreigns' = filter (isUsed' mn graph vertexFor entryPointVertices . foreignIdent) foreigns
    names = concatMap bindIdents ds' ++ map foreignIdent foreigns'
    exps' = filter (`elem` names) exps
  declarations = concatMap declarationsByModule ms
  (graph, _, vertexFor) = graphFromEdges $ map (\(key, deps) -> (key, key, deps)) declarations
  entryPointVertices = mapMaybe (vertexFor . fst) . filter (\((mn, _), _) -> mn `elem` entryPoints) $ declarations

-- |
-- Extract declaration names for a binding group.
--
bindIdents :: Bind a -> [Ident]
bindIdents (NonRec name _) = [name]
bindIdents (Rec names) = map fst names

-- |
-- Extract the ident for a foreign declaration.
--
foreignIdent :: ForeignDecl -> Ident
foreignIdent = fst

-- |
-- Key type to use in graph
--
type Key = (ModuleName, Ident)

-- |
-- Find dependencies for each member in a module.
--
declarationsByModule :: Module (Bind a) -> [(Key, [Key])]
declarationsByModule (Module _ mn _ _ fs ds) =
  let fs' = map ((\name -> ((mn, name), [])) . foreignIdent) fs
  in fs' ++ concatMap go ds
  where
  go :: Bind a -> [(Key, [Key])]
  go d@(NonRec name _) = [((mn, name), dependencies d)]
  go d@(Rec names') = map (\(name, _) -> ((mn, name), dependencies d)) names'

-- |
-- Find all referenced values within a binding group.
--
dependencies :: Bind a -> [Key]
dependencies =
  let (f, _, _, _) = everythingOnValues (++) (const []) values binders (const [])
  in nub . f
  where
  values :: Expr a -> [Key]
  values (Var _ (Qualified (Just mn) ident)) = [(mn, ident)]
  values _ = []
  binders :: Binder a -> [Key]
  binders (ConstructorBinder _ _ (Qualified (Just mn) ident) _) = [(mn, Ident $ runProperName ident)]
  binders _ = []

-- |
-- Check whether a binding group is used.
--
isUsed :: ModuleName -> Graph -> (Key -> Maybe Vertex) -> [Vertex] -> Bind a -> Bool
isUsed mn graph vertexFor entryPointVertices (NonRec name _) =
  isUsed' mn graph vertexFor entryPointVertices name
isUsed mn graph vertexFor entryPointVertices (Rec ds) =
  any (isUsed' mn graph vertexFor entryPointVertices . fst) ds

-- |
-- Check whether a named declaration is used.
--
isUsed' :: ModuleName -> Graph -> (Key -> Maybe Vertex) -> [Vertex] -> Ident -> Bool
isUsed' mn graph vertexFor entryPointVertices name =
  let Just v' = vertexFor (mn, name)
  in any (\v -> path graph v v') entryPointVertices
