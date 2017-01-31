{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Version (Version(..), showVersion)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)

import Options.Applicative (Parser, ParseError (..))
import qualified Options.Applicative as Opts

import System.IO (hSetEncoding, stderr, stdout, utf8)

import qualified Paths_purescript as Paths
import Language.PureScript.Publish
import Language.PureScript.Publish.ErrorsWarnings

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

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  Opts.execParser opts >>= publish
  where
  opts        = Opts.info (version <*> Opts.helper <*> dryRun) infoModList
  infoModList = Opts.fullDesc <> headerInfo <> footerInfo
  headerInfo  = Opts.header "psc-publish - Generates documentation packages for upload to http://pursuit.purescript.org"
  footerInfo  = Opts.footer $ "psc-publish " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = Opts.abortOption (InfoMsg (showVersion Paths.version)) $
    Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

publish :: Bool -> IO ()
publish isDryRun =
  if isDryRun
    then do
      _ <- unsafePreparePackage dryRunOptions
      putStrLn "Dry run completed, no errors."
    else do
      pkg <- unsafePreparePackage defaultPublishOptions
      BL.putStrLn (A.encode pkg)
