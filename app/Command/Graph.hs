{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Command.Graph (command) where

import           Control.Applicative (many)
import           Control.Monad (unless, when)
import qualified Data.Aeson as Json
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
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

  printWarningsAndErrors graphJSONErrors makeWarnings makeResult
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
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  case errors of
    Left _errs -> exitFailure
    Right res -> pure res


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
