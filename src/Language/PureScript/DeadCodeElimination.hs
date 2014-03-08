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

-- |
-- Eliminate all declarations which are not a transitive dependency of the entry point module
--
eliminateDeadCode :: [ModuleName] -> [Module] -> [Module]
eliminateDeadCode entryPoints ms = map go ms
  where
  go (Module moduleName ds (Just exps)) = Module moduleName ds' (Just exps')
    where
    ds' = filter (isUsed moduleName graph vertexFor entryPointVertices) ds
    exps' = mapMaybe (filterExport ds') exps
  go _ = error "Exports should have been elaborated in name desugaring"
  declarations = concatMap declarationsByModule ms
  (graph, _, vertexFor) = graphFromEdges $ map (\(key, deps) -> (key, key, deps)) declarations
  entryPointVertices = mapMaybe (vertexFor . fst) . filter (\((mn, _), _) -> mn `elem` entryPoints) $ declarations

  filterExport :: [Declaration] -> DeclarationRef -> Maybe DeclarationRef
  filterExport decls r@(TypeRef name _) | (any $ typeExists name) decls = Just r
  filterExport decls r@(ValueRef name) | (any $ valueExists name) decls = Just r
  filterExport decls r@(TypeInstanceRef name _ _) | (any $ valueExists name) decls = Just r
  filterExport _ _ = Nothing

  valueExists :: Ident -> Declaration -> Bool
  valueExists name (ValueDeclaration name' _ _ _ _) = name == name'
  valueExists name (ExternDeclaration _ name' _ _) = name == name'
  valueExists name (BindingGroupDeclaration decls) = any (\(name', _, _) -> name == name') decls
  valueExists _ _ = False

  typeExists :: ProperName -> Declaration -> Bool
  typeExists name (DataDeclaration name' _ _) = name == name'
  typeExists name (DataBindingGroupDeclaration decls) = any (typeExists name) decls
  typeExists _ _ = False 

type Key = (ModuleName, Either Ident ProperName)

declarationsByModule :: Module -> [(Key, [Key])]
declarationsByModule (Module moduleName ds _) = concatMap go ds
  where
  go :: Declaration -> [(Key, [Key])]
  go d@(ValueDeclaration name _ _ _ _) = [((moduleName, Left name), dependencies moduleName d)]
  go (DataDeclaration _ _ dctors) = map (\(name, _) -> ((moduleName, Right name), [])) dctors
  go (ExternDeclaration _ name _ _) = [((moduleName, Left name), [])]
  go d@(BindingGroupDeclaration names') = map (\(name, _, _) -> ((moduleName, Left name), dependencies moduleName d)) names'
  go (DataBindingGroupDeclaration ds') = concatMap go ds'
  go _ = []

dependencies :: (Data d) => ModuleName -> d -> [Key]
dependencies moduleName = nub . everything (++) (mkQ [] values)
  where
  values :: Value -> [Key]
  values (Var ident) = let (mn, name) = qualify moduleName ident in [(mn, Left name)]
  values (Constructor (Qualified (Just mn) name)) = [(mn, Right name)]
  values (Constructor (Qualified Nothing _)) = error "Found unqualified data constructor"
  values _ = []

isUsed :: ModuleName -> Graph -> (Key -> Maybe Vertex) -> [Vertex] -> Declaration -> Bool
isUsed moduleName graph vertexFor entryPointVertices (ValueDeclaration name _ _ _ _) =
  let Just v' = vertexFor (moduleName, Left name)
  in any (\v -> path graph v v') entryPointVertices
isUsed moduleName graph vertexFor entryPointVertices (DataDeclaration _ _ dctors) =
  any (\(pn, _) -> let Just v' = vertexFor (moduleName, Right pn)
                   in any (\v -> path graph v v') entryPointVertices) dctors
isUsed moduleName graph vertexFor entryPointVertices (ExternDeclaration _ name _ _) =
  let Just v' = vertexFor (moduleName, Left name)
  in any (\v -> path graph v v') entryPointVertices
isUsed moduleName graph vertexFor entryPointVertices (BindingGroupDeclaration ds) =
  any (\(name, _, _) -> let Just v' = vertexFor (moduleName, Left name)
                        in any (\v -> path graph v v') entryPointVertices) ds
isUsed moduleName graph vertexFor entryPointVertices (DataBindingGroupDeclaration ds) =
  any (isUsed moduleName graph vertexFor entryPointVertices) ds
isUsed _ _ _ _ _ = True
