{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer.Strict

import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.Map as M
import           Data.Version (showVersion)

import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make

import           Options.Applicative as Opts

import qualified Paths_purescript as Paths

import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.FilePath.Glob (glob)
import           System.IO (hSetEncoding, hPutStrLn, stdout, stderr, utf8)
import           System.IO.UTF8

data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [FilePath]
  , pscmOutputDir    :: FilePath
  , pscmOpts         :: P.Options
  , pscmUsePrefix    :: Bool
  , pscmJSONErrors   :: Bool
  }

-- | Argumnets: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . BU8.toString . B.toStrict . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: PSCMakeOptions -> IO ()
compile PSCMakeOptions{..} = do
  input <- globWarningOnMisses (unless pscmJSONErrors . warnFileTypeNotFound) pscmInput
  when (null input && not pscmJSONErrors) $ do
    hPutStrLn stderr "psc: No input files."
    exitFailure
  moduleFiles <- readInput input
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    ms <- P.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    P.make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
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

readInput :: [FilePath] -> IO [(FilePath, String)]
readInput inputFiles = forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8File inFile

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> Opts.value "output"
  <> showDefault
  <> help "The output directory"

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
                    <*> sourceMaps

pscMakeOptions :: Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pscMakeOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc - Compiles PureScript to Javascript"
  footerInfo  = footer $ "psc " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
