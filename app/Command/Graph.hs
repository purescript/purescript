module Command.Graph (command) where

import Prelude

import Control.Applicative (many)
import Control.Monad (unless, when)
import Data.Aeson qualified as Json
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.UTF8 qualified as LBU8
import Language.PureScript qualified as P
import Language.PureScript.Errors.JSON (JSONResult(..), toJSONErrors)
import Language.PureScript.Glob (PSCGlobs(..), toInputGlobs, warnFileTypeNotFound)
import Options.Applicative qualified as Opts
import SharedCLI qualified
import System.Console.ANSI qualified as ANSI
import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory)
import System.IO (hPutStr, hPutStrLn, stderr)

data GraphOptions = GraphOptions
  { graphInput       :: [FilePath]
  , graphInputFromFile :: Maybe FilePath
  , graphExclude       :: [FilePath]
  , graphJSONErrors  :: Bool
  }

graph :: GraphOptions -> IO ()
graph GraphOptions{..} = do
  input <- toInputGlobs $ PSCGlobs
    { pscInputGlobs = graphInput
    , pscInputGlobsFromFile = graphInputFromFile
    , pscExcludeGlobs = graphExclude
    , pscWarnFileTypeNotFound = unless graphJSONErrors . warnFileTypeNotFound "graph"
    }

  when (null input && not graphJSONErrors) $ do
    hPutStr stderr $ unlines
      [ "purs graph: No input files."
      , "Usage: For basic information, try the `--help' option."
      ]
    exitFailure

  (makeResult, makeWarnings) <- P.graph input

  printWarningsAndErrors graphJSONErrors makeWarnings makeResult
    >>= (LB.putStr . Json.encode)

command :: Opts.Parser (IO ())
command = graph <$> (Opts.helper <*> graphOptions)
  where
  graphOptions :: Opts.Parser GraphOptions
  graphOptions =
    GraphOptions <$> many SharedCLI.inputFile
                 <*> SharedCLI.globInputFile
                 <*> many SharedCLI.excludeFiles
                 <*> jsonErrors

  jsonErrors :: Opts.Parser Bool
  jsonErrors =
    Opts.switch $
      Opts.long "json-errors" <>
      Opts.help "Print errors to stderr as JSON"

-- | Arguments: use JSON, warnings, errors
printWarningsAndErrors :: Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO a
printWarningsAndErrors False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = True, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right res -> pure res
printWarningsAndErrors True warnings errors = do
  let verbose = True
  hPutStrLn stderr . LBU8.toString . Json.encode $
    JSONResult (toJSONErrors verbose P.Warning [] warnings)
               (either (toJSONErrors verbose P.Error []) (const []) errors)
  case errors of
    Left _errs -> exitFailure
    Right res -> pure res
