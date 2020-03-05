module Language.PureScript.Graph (graph) where

import Prelude.Compat

import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

import           Control.Monad (forM)
import           Data.Aeson ((.=))
import           Data.Foldable (foldl')
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           System.IO.UTF8 (readUTF8FileT)

import qualified Language.PureScript.Crash as Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Make as Make
import qualified Language.PureScript.ModuleDependencies as Dependencies
import qualified Language.PureScript.Options as Options

import           Language.PureScript.Errors (MultipleErrors)
import           Language.PureScript.Names (ModuleName, runModuleName)


-- | Given a set of filepaths, try to build the dependency graph and return
--   that as its JSON representation (or a bunch of errors, if any)
graph :: [FilePath] -> IO (Either MultipleErrors Json.Value, MultipleErrors)
graph input = do
  moduleFiles <- readInput input
  Make.runMake Options.defaultOptions $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let parsedModuleSig = Dependencies.moduleSignature . CST.resPartial
    (_sorted, moduleGraph) <- Dependencies.sortModules (parsedModuleSig . snd) ms
    let pathMap = Map.fromList $
          map (\(p, m) -> (Dependencies.sigModuleName (parsedModuleSig m), p)) ms
    pure (moduleGraphToJSON pathMap moduleGraph)

moduleGraphToJSON
  :: Map ModuleName FilePath
  -> Dependencies.ModuleGraph
  -> Json.Value
moduleGraphToJSON paths = Json.Object . foldl' insert mempty
  where
  insert :: Json.Object -> (ModuleName, [ModuleName]) -> Json.Object
  insert obj (mn, depends) = HashMap.insert (runModuleName mn) value obj
    where
      path = fromMaybe (Crash.internalError "missing module name in graph") $ Map.lookup mn paths
      value = Json.object
        [ "path"  .= path
        , "depends" .= fmap runModuleName depends
        ]

readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput inputFiles =
  forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8FileT inFile
