{-# LANGUAGE OverloadedStrings #-}

module Command.Publish (command) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Time.Clock (getCurrentTime)
import           Data.Version (Version(..))
import           Language.PureScript.Publish
import           Language.PureScript.Publish.ErrorsWarnings
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opts

data PublishOptionsCLI = PublishOptionsCLI
  { cliManifestPath :: FilePath
  , cliResolutionsPath :: FilePath
  , cliCompileOutputDir :: FilePath
  , cliDryRun :: Bool
  }

manifestPath :: Parser FilePath
manifestPath = Opts.strOption $
     Opts.long "manifest"
  <> Opts.metavar "FILE"
  <> Opts.help "The package manifest file"

resolutionsPath :: Parser FilePath
resolutionsPath = Opts.strOption $
     Opts.long "resolutions"
  <> Opts.metavar "FILE"
  <> Opts.help "The resolutions file"

dryRun :: Parser Bool
dryRun = Opts.switch $
     Opts.long "dry-run"
  <> Opts.help "Produce no output, and don't require a tagged version to be checked out."

compileOutputDir :: Opts.Parser FilePath
compileOutputDir = Opts.option Opts.auto $
     Opts.value "output"
  <> Opts.showDefault
  <> Opts.long "compile-output"
  <> Opts.metavar "DIR"
  <> Opts.help "Compiler output directory"

cliOptions :: Opts.Parser PublishOptionsCLI
cliOptions =
  PublishOptionsCLI <$> manifestPath <*> resolutionsPath <*> compileOutputDir <*> dryRun

mkPublishOptions :: PublishOptionsCLI -> PublishOptions
mkPublishOptions cliOpts =
  let
    opts =
      defaultPublishOptions
        { publishManifestFile = cliManifestPath cliOpts
        , publishResolutionsFile = cliResolutionsPath cliOpts
        , publishCompileOutputDir = cliCompileOutputDir cliOpts
        }
  in
    if cliDryRun cliOpts
      then
        opts
          { publishGetVersion = return ("0.0.0", Version [0,0,0] [])
          , publishGetTagTime = const (liftIO getCurrentTime)
          , publishWorkingTreeDirty = warn DirtyWorkingTree_Warn
          }
      else
        opts

command :: Opts.Parser (IO ())
command = publish <$> (Opts.helper <*> cliOptions)

publish :: PublishOptionsCLI -> IO ()
publish cliOpts = do
  let opts = mkPublishOptions cliOpts
  pkg <- unsafePreparePackage opts
  if cliDryRun cliOpts
    then putStrLn "Dry run completed, no errors."
    else BL.putStrLn (A.encode pkg)
