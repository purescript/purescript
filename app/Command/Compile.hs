{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Command.Compile (command) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer.Strict
import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           System.IO.UTF8 (readUTF8FileT)

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
  hPutStrLn stderr . BU8.toString . B.toStrict . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: PSCMakeOptions -> IO ()
compile PSCMakeOptions{..} = do
  input <- globWarningOnMisses (unless pscmJSONErrors . warnFileTypeNotFound) pscmInput
  when (null input && not pscmJSONErrors) $ do
    hPutStr stderr $ unlines [ "purs compile: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
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
warnFileTypeNotFound = hPutStrLn stderr . ("purs compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths
  concatMapM f = fmap concat . mapM f

readInput :: [FilePath] -> IO [(FilePath, Text)]
readInput inputFiles = forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8FileT inFile

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)"

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

comments :: Opts.Parser Bool
comments = Opts.switch $
     Opts.short 'c'
  <> Opts.long "comments"
  <> Opts.help "Include comments in the generated code"

verboseErrors :: Opts.Parser Bool
verboseErrors = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose-errors"
  <> Opts.help "Display verbose error messages"

noPrefix :: Opts.Parser Bool
noPrefix = Opts.switch $
     Opts.short 'p'
  <> Opts.long "no-prefix"
  <> Opts.help "Do not include comment header"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

sourceMaps :: Opts.Parser Bool
sourceMaps = Opts.switch $
     Opts.long "source-maps"
  <> Opts.help "Generate source maps"

dumpCoreFn :: Opts.Parser Bool
dumpCoreFn = Opts.switch $
     Opts.long "dump-corefn"
  <> Opts.help "Dump the (functional) core representation of the compiled code at output/*/corefn.json"

options :: Opts.Parser P.Options
options = P.Options <$> verboseErrors
                    <*> (not <$> comments)
                    <*> sourceMaps
                    <*> dumpCoreFn

pscMakeOptions :: Opts.Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> pscMakeOptions)
