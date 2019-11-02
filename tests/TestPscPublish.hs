{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TestPscPublish where

import Prelude

import Control.Exception (tryJust)
import Control.Monad (void, guard)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Aeson as A
import Data.Version
import Data.Foldable (forM_)
import qualified Text.PrettyPrint.Boxes as Boxes
import System.Directory (listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import Language.PureScript.Docs
import Language.PureScript.Publish (PublishOptions(..), defaultPublishOptions)
import qualified Language.PureScript.Publish as Publish
import qualified Language.PureScript.Publish.ErrorsWarnings as Publish

import Test.Tasty
import Test.Tasty.Hspec (Spec, Expectation, runIO, context, it, expectationFailure, testSpec)
import TestUtils hiding (inferForeignModules, makeActions)

main :: IO TestTree
main = testSpec "publish" spec

spec :: Spec
spec = do
  context "preparePackage with json roundtrips" $ do
    it "purescript-prelude" $ do
      testPackage
        "tests/support/bower_components/purescript-prelude"
        "../../prelude-resolutions.json"

    it "basic example" $ do
      testPackage
        "tests/purs/publish/basic-example"
        "resolutions.json"

    it "basic example with legacy resolutions file" $ do
      testPackage
        "tests/purs/publish/basic-example"
        "resolutions-legacy.json"

  context "json compatibility" $ do
    let compatDir = "tests" </> "json-compat"
    versions <- runIO $ listDirectory compatDir
    forM_ versions $ \version -> do
      context ("json produced by " ++ version) $ do
        files <- runIO $ listDirectory (compatDir </> version)
        forM_ files $ \file -> do
          it file $ do
            result <- A.eitherDecodeFileStrict' (compatDir </> version </> file)
            case result of
              Right (_ :: VerifiedPackage) ->
                pure ()
              Left err ->
                expectationFailure ("JSON parsing failed: " ++ err)

data TestResult
  = ParseFailed String
  | Mismatch ByteString ByteString -- ^ encoding before, encoding after
  | Pass ByteString
  deriving (Show)

roundTrip :: UploadedPackage -> TestResult
roundTrip pkg =
  let before = A.encode pkg
  in case A.eitherDecode before of
       Left err -> ParseFailed err
       Right parsed -> do
         let after' = A.encode (parsed :: UploadedPackage)
         if before == after'
           then Pass before
           else Mismatch before after'

testRunOptions :: FilePath -> PublishOptions
testRunOptions resolutionsFile = defaultPublishOptions
  { publishResolutionsFile = resolutionsFile
  , publishGetVersion = return testVersion
  , publishGetTagTime = const (liftIO getCurrentTime)
  , publishWorkingTreeDirty = return ()
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

-- | Given a directory which contains a package, produce JSON from it, and then
-- | attempt to parse it again, and ensure that it doesn't change.
testPackage :: FilePath -> FilePath -> Expectation
testPackage packageDir resolutionsFile = do
  res <- preparePackage packageDir resolutionsFile
  case res of
    Left err ->
      expectationFailure $
        "Failed to produce JSON from " ++ packageDir ++ ":\n" ++
        Boxes.render (Publish.renderError err)
    Right package ->
      case roundTrip package of
        Pass _ ->
          pure ()
        ParseFailed msg ->
          expectationFailure ("Failed to re-parse: " ++ msg)
        Mismatch _ _ ->
          expectationFailure "JSON did not match"

-- A version of Publish.preparePackage suitable for use in tests. We remove the
-- output directory each time to ensure that we are actually testing the docs
-- code in the working tree as it is now (as opposed to how it was at some
-- point in the past when the tests were previously successfully run).
preparePackage :: FilePath -> FilePath -> IO (Either Publish.PackageError UploadedPackage)
preparePackage packageDir resolutionsFile =
  pushd packageDir $ do
    removeDirectoryRecursiveIfPresent "output"
    Publish.preparePackage (testRunOptions resolutionsFile)

removeDirectoryRecursiveIfPresent :: FilePath -> IO ()
removeDirectoryRecursiveIfPresent =
  void . tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive
