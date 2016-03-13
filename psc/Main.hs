-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Strict

import Data.Bool (bool)
import Data.List (isSuffixOf, partition)
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8

import Options.Applicative as Opts

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.UTF8
import System.FilePath.Glob (glob)

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths

import Language.PureScript.Make

import JSON

data PSCMakeOptions = PSCMakeOptions
  { pscmInput             :: [FilePath]
  , pscmForeignInput      :: [FilePath]
  , pscmOutputDir         :: FilePath
  , pscmOpts              :: P.Options
  , pscmUsePrefix         :: Bool
  , pscmJSONErrors        :: Bool
  , pscmSummarizeWarnings :: Bool
  }

data InputOptions = InputOptions
  { ioInputFiles  :: [FilePath]
  }

-- | Argumnets: verbose, use JSON, warnings, errors
printWarningsAndErrors :: PSCMakeOptions -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors PSCMakeOptions{ pscmJSONErrors = False, .. } warnings errors = do
  when (P.nonEmpty warnings) $
    hPutStrLn stderr $
      if pscmSummarizeWarnings
        then (<> " warnings were discarded.") . show . length . P.runMultipleErrors $ warnings
        else P.prettyPrintMultipleWarnings (P.optionsVerboseErrors pscmOpts) warnings
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors pscmOpts) errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors PSCMakeOptions{..} warnings errors = do
  hPutStrLn stderr . BU8.toString . B.toStrict . A.encode $
    JSONResult (bool (JSONWarnings . toJSONErrors (P.optionsVerboseErrors pscmOpts) P.Warning)
                     (JSONWarningSummary . length . P.runMultipleErrors)
                     pscmSummarizeWarnings
                     warnings
               )
               (either (toJSONErrors (P.optionsVerboseErrors pscmOpts) P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: PSCMakeOptions -> IO ()
compile opts@PSCMakeOptions{..} = do
  input <- globWarningOnMisses (unless pscmJSONErrors . warnFileTypeNotFound) pscmInput
  when (null input && not pscmJSONErrors) $ do
    hPutStrLn stderr "psc: No input files."
    exitFailure
  let (jsFiles, pursFiles) = partition (isSuffixOf ".js") input
  moduleFiles <- readInput (InputOptions pursFiles)
  inputForeign <- globWarningOnMisses (unless pscmJSONErrors . warnFileTypeNotFound) pscmForeignInput
  foreignFiles <- forM (inputForeign ++ jsFiles) (\inFile -> (inFile,) <$> readUTF8File inFile)
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    (ms, foreigns) <- parseInputs moduleFiles foreignFiles
    let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, fp)) ms
        makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    P.make makeActions (map snd ms)
  printWarningsAndErrors opts makeWarnings makeErrors
  exitSuccess

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("psc: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern = do
    paths <- glob pattern
    when (null paths) $ warn pattern
    return paths
  concatMapM f = liftM concat . mapM f

readInput :: InputOptions -> IO [(Either P.RebuildPolicy FilePath, String)]
readInput InputOptions{..} = forM ioInputFiles $ \inFile -> (Right inFile, ) <$> readUTF8File inFile

parseInputs :: (MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)
            => [(Either P.RebuildPolicy FilePath, String)]
            -> [(FilePath, P.ForeignJS)]
            -> m ([(Either P.RebuildPolicy FilePath, P.Module)], M.Map P.ModuleName FilePath)
parseInputs modules foreigns =
  (,) <$> P.parseModulesFromFiles (either (const "") id) modules
      <*> P.parseForeignModulesFromFiles foreigns

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

inputForeignFile :: Parser FilePath
inputForeignFile = strOption $
     short 'f'
  <> long "ffi"
  <> help "The input .js file(s) providing foreign import implementations"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> Opts.value "output"
  <> showDefault
  <> help "The output directory"

requirePath :: Parser (Maybe FilePath)
requirePath = optional $ strOption $
     short 'r'
  <> long "require-path"
  <> help "The path prefix to use for require() calls in the generated JavaScript"

noTco :: Parser Bool
noTco = switch $
     long "no-tco"
  <> help "Disable tail call optimizations"

noMagicDo :: Parser Bool
noMagicDo = switch $
     long "no-magic-do"
  <> help "Disable the optimization that overloads the do keyword to generate efficient code specifically for the Eff monad"

noOpts :: Parser Bool
noOpts = switch $
     long "no-opts"
  <> help "Skip the optimization phase"

comments :: Parser Bool
comments = switch $
     short 'c'
  <> long "comments"
  <> help "Include comments in the generated code"

verboseErrors :: Parser Bool
verboseErrors = switch $
     short 'v'
  <> long "verbose-errors"
  <> help "Display verbose error messages"

noPrefix :: Parser Bool
noPrefix = switch $
     short 'p'
  <> long "no-prefix"
  <> help "Do not include comment header"

jsonErrors :: Parser Bool
jsonErrors = switch $
     long "json-errors"
  <> help "Print errors to stderr as JSON"

summarizeWarnings :: Parser Bool
summarizeWarnings = switch $
     long "summarize-warnings"
  <> help "Display a count of warnings instead of all warning information"

sourceMaps :: Parser Bool
sourceMaps = switch $
     long "source-maps"
  <> help "Generate source maps"

options :: Parser P.Options
options = P.Options <$> noTco
                    <*> noMagicDo
                    <*> pure Nothing
                    <*> noOpts
                    <*> verboseErrors
                    <*> (not <$> comments)
                    <*> requirePath
                    <*> sourceMaps

pscMakeOptions :: Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> many inputForeignFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors
                                <*> summarizeWarnings

main :: IO ()
main = execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pscMakeOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc - Compiles PureScript to Javascript"
  footerInfo  = footer $ "psc " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
