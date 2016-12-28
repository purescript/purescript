{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bundles compiled PureScript modules for the browser.
module Main (main) where

import Data.Traversable (for)
import Data.Version (showVersion)
import Data.Monoid ((<>))
import Data.Aeson (encode)
import Data.Maybe (isNothing)

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import System.FilePath (takeDirectory, (</>), (<.>), takeFileName)
import System.FilePath.Glob (glob)
import System.Exit (exitFailure)
import System.IO (stderr, stdout, hPutStrLn, hSetEncoding, utf8)
import System.IO.UTF8 (readUTF8File, writeUTF8File)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8

import Language.PureScript.Bundle

import Options.Applicative (Parser, ParseError (..))
import qualified Options.Applicative as Opts

import qualified Paths_purescript as Paths

import SourceMap
import SourceMap.Types

-- | Command line options.
data Options = Options
  { optionsInputFiles  :: [FilePath]
  , optionsOutputFile  :: Maybe FilePath
  , optionsEntryPoints :: [String]
  , optionsMainModule  :: Maybe String
  , optionsNamespace   :: String
  , optionsSourceMaps  :: Bool
  } deriving Show

-- | The main application function.
-- This function parses the input files, performs dead code elimination, filters empty modules
-- and generates and prints the final Javascript bundle.
app :: (MonadError ErrorMessage m, MonadIO m) => Options -> m (Maybe SourceMapping, String)
app Options{..} = do
  inputFiles <- concat <$> mapM (liftIO . glob) optionsInputFiles
  when (null inputFiles) . liftIO $ do
    hPutStrLn stderr "psc-bundle: No input files."
    exitFailure
  when (isNothing optionsOutputFile && optionsSourceMaps == True) . liftIO $ do
    hPutStrLn stderr "psc-bundle: Source maps only supported when output file specified."
    exitFailure

  input <- for inputFiles $ \filename -> do
    js <- liftIO (readUTF8File filename)
    mid <- guessModuleIdentifier filename
    length js `seq` return (mid, Just filename, js)                                            -- evaluate readFile till EOF before returning, not to exhaust file handles

  let entryIds = map (`ModuleIdentifier` Regular) optionsEntryPoints

  currentDir <- liftIO getCurrentDirectory
  let outFile = if optionsSourceMaps then fmap (currentDir </>) optionsOutputFile else Nothing
  bundleSM input entryIds optionsMainModule optionsNamespace outFile

-- | Command line options parser.
options :: Parser Options
options = Options <$> some inputFile
                  <*> optional outputFile
                  <*> many entryPoint
                  <*> optional mainModule
                  <*> namespace
                  <*> sourceMaps
  where
  inputFile :: Parser FilePath
  inputFile = Opts.strArgument $
       Opts.metavar "FILE"
    <> Opts.help "The input .js file(s)"

  outputFile :: Parser FilePath
  outputFile = Opts.strOption $
       Opts.short 'o'
    <> Opts.long "output"
    <> Opts.help "The output .js file"

  entryPoint :: Parser String
  entryPoint = Opts.strOption $
       Opts.short 'm'
    <> Opts.long "module"
    <> Opts.help "Entry point module name(s). All code which is not a transitive dependency of an entry point module will be removed."

  mainModule :: Parser String
  mainModule = Opts.strOption $
       Opts.long "main"
    <> Opts.help "Generate code to run the main method in the specified module."

  namespace :: Parser String
  namespace = Opts.strOption $
       Opts.short 'n'
    <> Opts.long "namespace"
    <> Opts.value "PS"
    <> Opts.showDefault
    <> Opts.help "Specify the namespace that PureScript modules will be exported to when running in the browser."

  sourceMaps :: Parser Bool
  sourceMaps = Opts.switch $
       Opts.long "source-maps"
    <> Opts.help "Whether to generate source maps for the bundle (requires --output)."

-- | Make it go.
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  opts <- Opts.execParser (Opts.info (version <*> Opts.helper <*> options) infoModList)
  output <- runExceptT (app opts)
  case output of
    Left err -> do
      hPutStrLn stderr (unlines (printErrorMessage err))
      exitFailure
    Right (sourcemap, js) ->
      case optionsOutputFile opts of
        Just outputFile -> do
          createDirectoryIfMissing True (takeDirectory outputFile)
          case sourcemap of
            Just sm -> do
              writeUTF8File outputFile $ js ++ "\n//# sourceMappingURL=" ++ (takeFileName outputFile <.> "map") ++ "\n"
              writeUTF8File (outputFile <.> "map") $ BU8.toString . B.toStrict . encode $ generate sm
            Nothing -> writeUTF8File outputFile js
        Nothing -> putStrLn js
  where
  infoModList = Opts.fullDesc <> headerInfo <> footerInfo
  headerInfo  = Opts.header   "psc-bundle - Bundles compiled PureScript modules for the browser"
  footerInfo  = Opts.footer $ "psc-bundle " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = Opts.abortOption (InfoMsg (showVersion Paths.version)) $
    Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden
