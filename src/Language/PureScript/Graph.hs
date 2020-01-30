module Language.PureScript.Graph (graph) where

import Prelude.Compat

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Aeson as Json

import           Control.Monad (forM, foldM)
import           Data.Aeson ((.=))
import           Data.Map (Map)
import           Data.ByteString.Lazy.UTF8 (ByteString)
import           Data.Text (Text)
import           System.IO.UTF8 (readUTF8FileT)

import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Make as Make
import qualified Language.PureScript.ModuleDependencies as Dependencies
import qualified Language.PureScript.Options as Options

import           Language.PureScript.Errors (MultipleErrors)
import           Language.PureScript.Names (ModuleName, runModuleName)


graph :: [FilePath] -> IO (MultipleErrors, Either MultipleErrors ByteString)
graph input = do
  moduleFiles <- readInput input
  (makeResult, makeWarnings) <- Make.runMake Options.defaultOptions $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let parsedModuleSig = Dependencies.moduleSignature . CST.resPartial
    (_sorted, moduleGraph) <- Dependencies.sortModules (parsedModuleSig . snd) ms
    let swap (a, b) = (b, a)
    let pathMap = Map.fromList
                . fmap (swap . fmap (Dependencies.sigModuleName . parsedModuleSig))
                $ ms
    pure (moduleGraphToJSON pathMap moduleGraph)
  pure $ case makeResult of
    Left makeErrors -> (makeWarnings, Left makeErrors)
    Right Nothing -> undefined -- TODO: what should happen here?
    Right (Just json) -> (makeWarnings, Right $ Json.encode json)


moduleGraphToJSON
  :: Map ModuleName FilePath -> Dependencies.ModuleGraph -> Maybe Json.Value
moduleGraphToJSON paths = fmap Json.Object . foldM insert mempty
  where
  insert :: Json.Object -> (ModuleName, [ModuleName]) -> Maybe Json.Object
  insert obj (mn, depends) = flip (HashMap.insert key) obj <$> value
    where
    key :: Text
    key = runModuleName mn

    value :: Maybe Json.Value
    value = do
      path <- Map.lookup mn paths
      pure $ Json.object
        [ "path"  .= path
        , "depends" .= fmap runModuleName depends
        ]


readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput inputFiles =
  forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8FileT inFile
