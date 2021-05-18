module Command.Codegen (command) where

import Prelude

import           Control.Applicative (many)
import           Control.Monad (when, unless)
import qualified Language.PureScript as P
import qualified Options.Applicative as Opts
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)

data CodegenOptions = CodegenOptions
  { codegenCoreFnInput :: [FilePath]
  , codegenJSONErrors :: Bool
  }

codegen :: CodegenOptions -> IO ()
codegen CodegenOptions{..} = do
  input <- globWarningOnMisses (unless codegenJSONErrors . warnFileTypeNotFound) codegenCoreFnInput
  when (null input && not codegenJSONErrors) $ do
    hPutStr stderr $ unlines
      [ "purs codegen: No input files."
      , "Usage: For basic information, try the `--help` option."
      ]
    exitFailure

  exitSuccess
  where
  warnFileTypeNotFound :: String -> IO ()
  warnFileTypeNotFound =
    hPutStrLn stderr . ("purs graph: No files found using pattern: " <>)

command :: Opts.Parser (IO ())
command = codegen <$> (Opts.helper <*> codegenOptions)
  where
  codegenOptions :: Opts.Parser CodegenOptions
  codegenOptions =
    CodegenOptions <$> many inputFile
                   <*> jsonErrors

  inputFile :: Opts.Parser FilePath
  inputFile =
    Opts.strArgument $
      Opts.metavar "FILE" <>
      Opts.help "The input corefn.json file(s)."

  jsonErrors :: Opts.Parser Bool
  jsonErrors =
    Opts.switch $
      Opts.long "json-errors" <>
      Opts.help "Print errors to stderr as JSON"

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
