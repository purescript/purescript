{-# LANGUAGE OverloadedStrings #-}

module Command.Publish (command) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Time.Clock (getCurrentTime)
import           Data.Version (Version(..))
import           Language.PureScript.Docs
import           Language.PureScript.Publish
import           Language.PureScript.Publish.ErrorsWarnings
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opts

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

nonPublic :: Parser Bool
nonPublic = Opts.switch $
     Opts.long "non-public"
  <> Opts.help "Produce output, don't check version or license.  Blank user and version 0.0.0"

dryRunOptions :: PublishOptions
dryRunOptions = defaultPublishOptions
  { publishGetVersion = return dummyVersion
  , publishWorkingTreeDirty = warn DirtyWorkingTree_Warn
  , publishGetTagTime = const (liftIO getCurrentTime)
  }
  where dummyVersion = ("0.0.0", Version [0,0,0] [])

nonPublicOptions :: PublishOptions
nonPublicOptions = defaultPublishOptions
  { publishGetVersion = return dummyVersion
  , publishWorkingTreeDirty = warn DirtyWorkingTree_Warn
  , publishGetTagTime = const (liftIO getCurrentTime)
  , publishCheckLicense = \_ -> pure ()
  , publishGetRepoInfo = \_ -> pure (GithubUser "", GithubRepo "")
  }
  where dummyVersion = ("0.0.0", Version [0,0,0] [])

command :: Opts.Parser (IO ())
command = publish <$> manifestPath <*> resolutionsPath <*> (Opts.helper <*> dryRun) <*> (Opts.helper <*> nonPublic)

publish :: FilePath -> FilePath -> Bool -> Bool -> IO ()
publish manifestFile resolutionsFile isDryRun isNonPublic =
  if isNonPublic
    then do
      pkg <- unsafePreparePackage manifestFile resolutionsFile nonPublicOptions
      BL.putStrLn (A.encode (verifyPackage (GithubUser "") pkg))
    else if isDryRun
      then do
        _ <- unsafePreparePackage manifestFile resolutionsFile dryRunOptions
        putStrLn "Dry run completed, no errors."
      else do
        pkg <- unsafePreparePackage manifestFile resolutionsFile defaultPublishOptions
        BL.putStrLn (A.encode pkg)
