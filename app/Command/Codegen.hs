module Command.Codegen (command) where

import Prelude

import           Command.Common (globWarningOnMisses, printWarningsAndErrors)

import           Control.Applicative (many)
import           Control.Monad (when, unless)
import           Control.Monad.Supply

import qualified Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (lefts, rights)
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import qualified Language.PureScript.Docs.Types as Docs

import qualified Options.Applicative as Opts

import           System.Exit (exitFailure)
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
    P.runMake purescriptOptions
      $ runSupplyT 0
      $ traverse (runCodegen foreigns filePathMap . snd) $ rights mods
  printWarningsAndErrors True codegenJSONErrors makeWarnings makeResult
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
