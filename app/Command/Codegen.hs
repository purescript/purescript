module Command.Codegen (command) where

import Prelude

import           Control.Applicative (many)
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Supply

import qualified Data.Aeson as Json
import qualified Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.Either (lefts, rights)
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Options.Applicative as Opts

import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import           Language.PureScript.Errors.JSON
import qualified Language.PureScript.Docs.Types as Docs

import qualified System.Console.ANSI as ANSI
import           System.Directory (getCurrentDirectory)
import           System.Exit (exitFailure)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)

data CodegenOptions = CodegenOptions
  { codegenCoreFnInput :: [FilePath]
  , codegenJSONErrors :: Bool
  , codegenOutputDir :: FilePath
  }

codegen :: CodegenOptions -> IO ()
codegen CodegenOptions{..} = do
  inputFiles <- globWarningOnMisses (unless codegenJSONErrors . warnFileTypeNotFound) codegenCoreFnInput
  when (null inputFiles && not codegenJSONErrors) $ do
    hPutStr stderr $ unlines
      [ "purs codegen: No input files."
      , "Usage: For basic information, try the `--help` option."
      ]
    exitFailure
  mods <- traverse parseCoreFn inputFiles

  let
    filePathMap =
      M.fromList $ map ((\m -> (CoreFn.moduleName m, Right $ CoreFn.modulePath m)) . snd) $ rights mods

  unless (null (lefts mods)) $ do
    _ <- traverse (hPutStr stderr . formatParseError) $ lefts mods
    exitFailure

  foreigns <- P.inferForeignModules filePathMap
  (makeResult, makeWarnings) <-
    liftIO
      $ P.runMake purescriptOptions
      $ runSupplyT 0
      $ traverse (runCodegen foreigns filePathMap . snd) $ rights mods
  printWarningsAndErrors codegenJSONErrors makeWarnings makeResult
  where
  formatParseError (file, _, e) =
    "Failed parsing file " <> file <> " with error: " <> e

  parseCoreFn file = do
    contents <- BSL.readFile file
    case eitherDecodeWith json (A.iparse CoreFn.moduleFromJSON) contents of
      Left (j, e) -> pure $ Left (file, j, e)
      Right r -> pure $ Right r

  makeActions foreigns filePathMap = P.buildMakeActions codegenOutputDir filePathMap foreigns False

  purescriptOptions :: P.Options
  purescriptOptions = P.Options False False (S.fromList [ P.JS ])

  runCodegen foreigns filePathMap m =
    P.codegen (makeActions foreigns filePathMap) m
      (Docs.Module (CoreFn.moduleName m) Nothing [] [])
      Nothing

  warnFileTypeNotFound :: String -> IO ()
  warnFileTypeNotFound =
    hPutStrLn stderr . ("purs codegen: No files found using pattern: " <>)

command :: Opts.Parser (IO ())
command = codegen <$> (Opts.helper <*> codegenOptions)
  where
  codegenOptions :: Opts.Parser CodegenOptions
  codegenOptions =
    CodegenOptions <$> many inputFile
                   <*> jsonErrors
                   <*> outputDirectory

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

  outputDirectory :: Opts.Parser FilePath
  outputDirectory = Opts.strOption $
    Opts.short 'o'
      <> Opts.long "output"
      <> Opts.value "output"
      <> Opts.showDefault
      <> Opts.help "The output directory"

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

-- | Arguments: use JSON, warnings, errors
printWarningsAndErrors :: Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
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
    Right _ -> pure ()
printWarningsAndErrors True warnings errors = do
  let verbose = True
  hPutStrLn stderr . LBU8.toString . Json.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  case errors of
    Left _errs -> exitFailure
    Right _ -> pure ()
