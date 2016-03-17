-----------------------------------------------------------------------------
--
-- Module      :  psc-bundle
-- Copyright   :  (c) Phil Freeman 2015
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Bundles compiled PureScript modules for the browser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Maybe 
import Data.Traversable (for)
import Data.Version (showVersion)

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import System.FilePath (takeFileName, takeDirectory)
import System.FilePath.Glob (glob)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import System.Directory (createDirectoryIfMissing)

import Language.PureScript.Bundle

import Options.Applicative as Opts

import qualified Paths_purescript as Paths

-- | Command line options.
data Options = Options
  { optionsInputFiles  :: [FilePath]
  , optionsOutputFile  :: Maybe FilePath
  , optionsEntryPoints :: [String]
  , optionsMainModule  :: Maybe String
  , optionsNamespace   :: String
  , optionsRequirePath :: Maybe FilePath
  } deriving Show

-- | Given a filename, assuming it is in the correct place on disk, infer a ModuleIdentifier.
guessModuleIdentifier :: (MonadError ErrorMessage m) => FilePath -> m ModuleIdentifier
guessModuleIdentifier filename = ModuleIdentifier (takeFileName (takeDirectory filename)) <$> guessModuleType (takeFileName filename)
  where
  guessModuleType "index.js" = pure Regular
  guessModuleType "foreign.js" = pure Foreign
  guessModuleType name = throwError $ UnsupportedModulePath name

-- | The main application function.
-- This function parses the input files, performs dead code elimination, filters empty modules
-- and generates and prints the final Javascript bundle.
app :: (MonadError ErrorMessage m, MonadIO m) => Options -> m String
app Options{..} = do
  inputFiles <- concat <$> mapM (liftIO . glob) optionsInputFiles
  when (null inputFiles) . liftIO $ do
    hPutStrLn stderr "psc-bundle: No input files."
    exitFailure
  input <- for inputFiles $ \filename -> do
    js <- liftIO (readFile filename)
    mid <- guessModuleIdentifier filename
    length js `seq` return (mid, js)                                            -- evaluate readFile till EOF before returning, not to exhaust file handles

  let entryIds = map (`ModuleIdentifier` Regular) optionsEntryPoints

  bundle input entryIds optionsMainModule optionsNamespace optionsRequirePath

-- | Command line options parser.
options :: Parser Options
options = Options <$> some inputFile
                  <*> optional outputFile
                  <*> many entryPoint
                  <*> optional mainModule
                  <*> namespace
                  <*> optional requirePath
  where
  inputFile :: Parser FilePath
  inputFile = strArgument $
       metavar "FILE"
    <> help "The input .js file(s)"

  outputFile :: Parser FilePath
  outputFile = strOption $
       short 'o'
    <> long "output"
    <> help "The output .js file"

  entryPoint :: Parser String
  entryPoint = strOption $
       short 'm'
    <> long "module"
    <> help "Entry point module name(s). All code which is not a transitive dependency of an entry point module will be removed."

  mainModule :: Parser String
  mainModule = strOption $
       long "main"
    <> help "Generate code to run the main method in the specified module."

  namespace :: Parser String
  namespace = strOption $
       short 'n'
    <> long "namespace"
    <> Opts.value "PS"
    <> showDefault
    <> help "Specify the namespace that PureScript modules will be exported to when running in the browser."

  requirePath :: Parser FilePath
  requirePath = strOption $
       short 'r'
    <> long "require-path"
    <> help "The path prefix used in require() calls in the generated JavaScript [deprecated]"

-- | Make it go.
main :: IO ()
main = do
  opts <- execParser (info (version <*> helper <*> options) infoModList)
  when (isJust (optionsRequirePath opts)) $ hPutStrLn stderr "The require-path option is deprecated and will be removed in PureScript 0.9."
  output <- runExceptT (app opts)
  case output of
    Left err -> do
      hPutStrLn stderr (unlines (printErrorMessage err))
      exitFailure
    Right js ->
      case optionsOutputFile opts of
        Just outputFile -> do
          createDirectoryIfMissing True (takeDirectory outputFile)
          writeFile outputFile js
        Nothing -> putStrLn js
  where
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-bundle - Bundles compiled PureScript modules for the browser"
  footerInfo  = footer $ "psc-bundle " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
