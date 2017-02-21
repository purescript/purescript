{-# LANGUAGE OverloadedStrings #-}

module Command.Publish (command) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import           Data.Time.Clock (getCurrentTime)
import           Data.Version (Version(..))
import           Language.PureScript.Publish
import           Language.PureScript.Publish.ErrorsWarnings
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opts

manifestPath :: Parser FilePath
manifestPath = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The package manifest file"

dryRun :: Parser Bool
dryRun = Opts.switch $
     Opts.long "dry-run"
  <> Opts.help "Produce no output, and don't require a tagged version to be checked out."

dryRunOptions :: PublishOptions
dryRunOptions = defaultPublishOptions
  { publishGetVersion = return dummyVersion
  , publishWorkingTreeDirty = warn DirtyWorkingTree_Warn
  , publishGetTagTime = const (liftIO getCurrentTime)
  }
  where dummyVersion = ("0.0.0", Version [0,0,0] [])

command :: Opts.Parser (IO ())
command = publish <$> manifestPath <*> (Opts.helper <*> dryRun)

publish :: FilePath -> Bool -> IO ()
publish manifest isDryRun =
  if isDryRun
    then do
      _ <- unsafePreparePackage manifest dryRunOptions
      putStrLn "Dry run completed, no errors."
    else do
      pkg <- unsafePreparePackage manifest defaultPublishOptions
      BL.putStrLn (A.encode pkg)
