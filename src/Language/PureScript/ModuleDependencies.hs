-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( sortModules
  , ModuleGraph
  ) where

import           Protolude

import           Data.Graph
import qualified Data.Set as S
import           Language.PureScript.AST
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Errors
import           Language.PureScript.Names

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules
  :: forall m
   . MonadError MultipleErrors m
  => [Module]
  -> m ([Module], ModuleGraph)
sortModules ms = do
    let mns = S.fromList $ map getModuleName ms
    verts <- mapM (toGraphNode mns) ms
    ms' <- mapM toModule $ stronglyConnComp verts
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (ms', moduleGraph)
  where
    toGraphNode :: S.Set ModuleName -> Module -> m (Module, ModuleName, [ModuleName])
    toGraphNode mns m@(Module _ _ mn ds _) = do
      let deps = ordNub (concatMap usedModules ds)
      forM_ deps $ \dep ->
        when (dep /= C.Prim && S.notMember dep mns) $
          throwError . addHint (ErrorInModule mn) . errorMessage $ ModuleNotFound dep
      pure (m, getModuleName m, deps)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> [ModuleName]
usedModules d = f d where
  f :: Declaration -> [ModuleName]
  (f, _, _, _, _) = everythingOnValues (++) forDecls (const []) (const []) (const []) (const [])

  forDecls :: Declaration -> [ModuleName]
  -- Regardless of whether an imported module is qualified we still need to
  -- take into account its import to build an accurate list of dependencies.
  forDecls (ImportDeclaration mn _ _) = [mn]
  forDecls _ = []

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => SCC Module -> m Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC [m]) = return m
toModule (CyclicSCC ms) = throwError . errorMessage $ CycleInModules (map getModuleName ms)
