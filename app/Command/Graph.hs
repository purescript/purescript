{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Command.Graph (command) where

import           Control.Applicative (many)
import           Control.Monad (unless, when, forM, foldM)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.Bool (bool)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.Text (Text)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make (runMake)
import           Language.PureScript.ModuleDependencies
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           System.IO.UTF8 (readUTF8FileT)

data GraphOptions = GraphOptions
  { graphInput      :: [FilePath]
  , graphJSONErrors :: Bool
  }

graph :: GraphOptions -> IO ()
graph GraphOptions{..} = do
  input <- globWarningOnMisses (unless graphJSONErrors . warnFileTypeNotFound) graphInput
  when (null input && not graphJSONErrors) $ do
    hPutStr stderr $ unlines
      [ "purs graph: No input files."
      , "Usage: For basic information, try the `--help' option."
      ]
    exitFailure

  moduleFiles <- readInput input
  (makeResult, makeWarnings) <- runMake P.defaultOptions $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let parsedModuleSig = moduleSignature . CST.resPartial
    (_sorted, moduleGraph) <- sortModules (parsedModuleSig . snd) ms
    let swap (a, b) = (b, a)
    let pathMap = M.fromList
                . fmap (swap . fmap (sigModuleName . parsedModuleSig))
                $ ms
    pure (moduleGraphToJSON pathMap moduleGraph)

  printWarningsAndErrors True graphJSONErrors makeWarnings makeResult
  case makeResult of
    Left _ -> exitSuccess
    Right Nothing -> undefined
    Right (Just json) -> LB.putStr $ A.encode json

  where
  warnFileTypeNotFound :: String -> IO ()
  warnFileTypeNotFound =
    hPutStrLn stderr . ("purs compile: No files found using pattern: " <>)

command :: Opts.Parser (IO ())
command = graph <$> (Opts.helper <*> graphOptions)
  where
  graphOptions :: Opts.Parser GraphOptions
  graphOptions =
    GraphOptions <$> many inputFile
                 <*> jsonErrors

  inputFile :: Opts.Parser FilePath
  inputFile =
    Opts.strArgument $
      Opts.metavar "FILE" <>
      Opts.help "The input .purs file(s)."

  jsonErrors :: Opts.Parser Bool
  jsonErrors =
    Opts.switch $
      Opts.long "json-errors" <>
      Opts.help "Print errors to stderr as JSON"

moduleGraphToJSON
  :: M.Map P.ModuleName FilePath -> ModuleGraph -> Maybe A.Value
moduleGraphToJSON paths = fmap A.Object . foldM insert mempty
  where
  insert :: A.Object -> (P.ModuleName, [P.ModuleName]) -> Maybe A.Object
  insert obj (mn, depends) = flip (HM.insert key) obj <$> value
    where
    key :: Text
    key = P.runModuleName mn

    value :: Maybe A.Value
    value = do
      path <- M.lookup mn paths
      pure $ A.object
        [ "path"  .= path
        , "depends" .= fmap P.runModuleName depends
        ]

-- | Arguments: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput inputFiles =
  forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8FileT inFile

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning :: String -> IO [FilePath]
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths

  concatMapM :: (a -> IO [b]) -> [a] -> IO [b]
  concatMapM f = fmap concat . mapM f
