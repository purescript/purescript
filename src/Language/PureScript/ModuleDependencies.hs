-- | Provides the ability to sort modules based on module dependencies
module Language.PureScript.ModuleDependencies
  ( sortModules
  , ModuleGraph
  , ModuleSignature(..)
  , moduleSignature
  ) where

import           Protolude hiding (head)

import           Data.Graph
import qualified Data.Set as S
import           Language.PureScript.AST
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Errors hiding (nonEmpty)
import           Language.PureScript.Names

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | A module signature for sorting dependencies.
data ModuleSignature = ModuleSignature
  { sigSourceSpan :: SourceSpan
  , sigModuleName :: ModuleName
  , sigImports :: [(ModuleName, SourceSpan)]
  }

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
sortModules
  :: forall m a
   . MonadError MultipleErrors m
  => (a -> ModuleSignature)
  -> [a]
  -> m ([a], ModuleGraph)
sortModules toSig ms = do
    let
      ms' = (\m -> (m, toSig m)) <$> ms
      mns = S.fromList $ map (sigModuleName . snd) ms'
    verts <- parU ms' (toGraphNode mns)
    ms'' <- parU (stronglyConnComp verts) toModule
    let (graph, fromVertex, toVertex) = graphFromEdges verts
        moduleGraph = do (_, mn, _) <- verts
                         let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                             deps    = reachable graph v
                             toKey i = case fromVertex i of (_, key, _) -> key
                         return (mn, filter (/= mn) (map toKey deps))
    return (fst <$> ms'', moduleGraph)
  where
    toGraphNode :: S.Set ModuleName -> (a, ModuleSignature) -> m ((a, ModuleSignature), ModuleName, [ModuleName])
    toGraphNode mns m@(_, ModuleSignature _ mn deps) = do
      void . parU deps $ \(dep, pos) ->
        when (dep `notElem` C.primModules && S.notMember dep mns) .
          throwError
            . addHint (ErrorInModule mn)
            . errorMessage' pos
            $ ModuleNotFound dep
      pure (m, mn, map fst deps)

-- | Calculate a list of used modules based on explicit imports and qualified names.
usedModules :: Declaration -> Maybe (ModuleName, SourceSpan)
-- Regardless of whether an imported module is qualified we still need to
-- take into account its import to build an accurate list of dependencies.
usedModules (ImportDeclaration (ss, _) mn _ _) = pure (mn, ss)
usedModules _ = Nothing

-- | Convert a strongly connected component of the module graph to a module
toModule :: MonadError MultipleErrors m => SCC (a, ModuleSignature) -> m (a, ModuleSignature)
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC ms) =
  case nonEmpty ms of
    Nothing ->
      internalError "toModule: empty CyclicSCC"
    Just ms' ->
      throwError
        . errorMessage'' (fmap (sigSourceSpan . snd) ms')
        $ CycleInModules (map (sigModuleName . snd) ms)

moduleSignature :: Module -> ModuleSignature
moduleSignature (Module ss _ mn ds _) = ModuleSignature ss mn (ordNub (mapMaybe usedModules ds))
