-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( sortModules
  , ModuleGraph
  ) where

import           Protolude hiding (head)

import           Data.Graph
import           Data.List.NonEmpty (NonEmpty((:|)))
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
    verts <- parU ms (toGraphNode mns)
    ms' <- parU (stronglyConnComp verts) toModule
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
      let deps = ordNub (mapMaybe usedModules ds)
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` [C.Prim, C.PrimRow, C.PrimTypeError] && S.notMember dep mns) .
          throwError
            . addHint (ErrorInModule mn)
            . errorMessage' pos
            $ ModuleNotFound dep
      pure (m, getModuleName m, map fst deps)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => SCC Module -> m Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC []) = internalError "toModule: empty CyclicSCC"
toModule (CyclicSCC [m]) = return m
toModule (CyclicSCC (m : ms)) =
  throwError
    . errorMessage'' (fmap getModuleSourceSpan (m :| ms))
    $ CycleInModules (map getModuleName ms)
