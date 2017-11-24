{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bundles compiled PureScript modules for the browser.
module Command.Bundle (command) where

import           PSPrelude hiding ((<.>))

import           Data.Aeson (encode)
import           Data.List (concat, null)

import qualified Data.Text as T
import           Data.Traversable (for)
import           System.FilePath (takeDirectory, (</>), (<.>), takeFileName)
import           System.FilePath.Glob (glob)
import           System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import           Language.PureScript.Bundle
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opts
import           SourceMap
import           SourceMap.Types

-- | Command line options.
data Options = Options
  { optionsInputFiles  :: [FilePath]
  , optionsOutputFile  :: Maybe FilePath
  , optionsEntryPoints :: [Text]
  , optionsMainModule  :: Maybe Text
  , optionsNamespace   :: Text
  , optionsSourceMaps  :: Bool
  } deriving Show


-- | The main application function.
-- This function parses the input files, performs dead code elimination, filters empty modules
-- and generates and prints the final Javascript bundle.
app :: (MonadError ErrorMessage m, MonadIO m) => Options -> m (Maybe SourceMapping, Text)
app Options{..} = do
  inputFiles <- concat <$> mapM (liftIO . glob) optionsInputFiles
  when (null inputFiles) . liftIO $ do
    fatal "purs bundle: No input files."
  when (isNothing optionsOutputFile && optionsSourceMaps == True) . liftIO $ do
    fatal "purs bundle: Source maps only supported when output file specified."

  input <- for inputFiles $ \filename -> do
    js <- liftIO (readFile filename)
    mid <- guessModuleIdentifier filename
    T.length js `seq` return (mid, Just filename, js)                                            -- evaluate readFile till EOF before returning, not to exhaust file handles

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

  entryPoint :: Parser Text
  entryPoint = Opts.strOption $
       Opts.short 'm'
    <> Opts.long "module"
    <> Opts.help "Entry point module name(s). All code which is not a transitive dependency of an entry point module will be removed."

  mainModule :: Parser Text
  mainModule = Opts.strOption $
       Opts.long "main"
    <> Opts.help "Generate code to run the main method in the specified module."

  namespace :: Parser Text
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
command :: Opts.Parser (IO ())
command = run <$> (Opts.helper <*> options) where
  run :: Options -> IO ()
  run opts = do
    output <- runExceptT (app opts)
    case output of
      Left err -> do
        putErrTextLines (printErrorMessage err)
        exitFailure
      Right (sourcemap, js) ->
        case optionsOutputFile opts of
          Just outputFile -> do
            createDirectoryIfMissing True (takeDirectory outputFile)
            case sourcemap of
              Just sm -> do
                writeFile outputFile $ js <> "\n//# sourceMappingURL=" <> (toS (takeFileName outputFile <.> "map")) <> "\n"
                writeFile (outputFile <.> "map") $ toS . encode $ generate sm
              Nothing -> writeFile outputFile js
          Nothing -> putStrLn js
