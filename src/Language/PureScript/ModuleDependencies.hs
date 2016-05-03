{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Provides the ability to sort modules based on module dependencies
--
module Language.PureScript.ModuleDependencies (
  sortModules,
  ModuleGraph
) where

import Control.Monad.Error.Class (MonadError(..))

import Data.Graph
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
--
sortModules
  :: forall m header
   . (MonadError MultipleErrors m)
  => (header -> ModuleHeader)
  -> [header]
  -> m ([header], ModuleGraph)
sortModules toModuleHeader hdrs = do
    let verts = map goModule hdrs
    ms' <- mapM toModule $ stronglyConnComp verts
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (ms', moduleGraph)
  where
  goModule :: header -> (header, ModuleName, [ModuleName])
  goModule m = (m, moduleHeaderName mh, nub (mapMaybe usedModules (moduleHeaderImports mh)))
    where
      mh = toModuleHeader m

  -- | Calculate a list of used modules based on explicit imports.
  usedModules :: Declaration -> Maybe ModuleName
  usedModules (ImportDeclaration mn _ _) = Just mn
  usedModules _ = Nothing

  -- | Convert a strongly connected component of the module graph to a module
  toModule :: SCC header -> m header
  toModule (AcyclicSCC m) = return m
  toModule (CyclicSCC [m]) = return m
  toModule (CyclicSCC ms) = throwError . errorMessage $ CycleInModules (map (moduleHeaderName . toModuleHeader) ms)
