
module Main where

import Data.Version (Version(..), showVersion)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL

import Options.Applicative hiding (str)

import System.IO (hSetEncoding, stderr, stdout, utf8)

import qualified Paths_purescript as Paths
import Language.PureScript.Publish
import Language.PureScript.Publish.ErrorsWarnings

dryRun :: Parser Bool
dryRun = switch $
     long "dry-run"
  <> help "Produce no output, and don't require a tagged version to be checked out."

dryRunOptions :: PublishOptions
dryRunOptions = defaultPublishOptions
  { publishGetVersion = return dummyVersion
  , publishWorkingTreeDirty = warn DirtyWorkingTree_Warn
  }
  where dummyVersion = ("0.0.0", Version [0,0,0] [])

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  execParser opts >>= publish
  where
  opts        = info (version <*> helper <*> dryRun) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header "psc-publish - Generates documentation packages for upload to http://pursuit.purescript.org"
  footerInfo  = footer $ "psc-publish " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden

publish :: Bool -> IO ()
publish isDryRun =
  if isDryRun
    then do
      _ <- unsafePreparePackage dryRunOptions
      putStrLn "Dry run completed, no errors."
    else do
      pkg <- unsafePreparePackage defaultPublishOptions
      BL.putStrLn (A.encode pkg)
