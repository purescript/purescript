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
import           System.Exit (exitSuccess, exitFailure)
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

  (makeWarnings, makeResult) <- P.graph input

  printWarningsAndErrors True graphJSONErrors makeWarnings makeResult

  case makeResult of
    Left _ -> exitSuccess
    Right result -> LB.putStr result

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
  hPutStrLn stderr . LBU8.toString . Json.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors


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
