module Command.Graph (command) where

import Prelude

import           Command.Common (globWarningOnMisses, printWarningsAndErrors)
import           Control.Applicative (many)
import           Control.Monad (unless, when)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LB
import qualified Language.PureScript as P
import qualified Options.Applicative as Opts
import           System.Exit (exitFailure)
import           System.IO (hPutStr, hPutStrLn, stderr)

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

  (makeResult, makeWarnings) <- P.graph input

  printWarningsAndErrors True graphJSONErrors makeWarnings makeResult
    >>= (LB.putStr . Json.encode)

  where
  warnFileTypeNotFound :: String -> IO ()
  warnFileTypeNotFound =
    hPutStrLn stderr . ("purs graph: No files found using pattern: " <>)


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
