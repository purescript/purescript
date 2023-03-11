module TestPscPublish where

import Prelude

import Control.Exception (tryJust)
import Control.Monad (void, guard)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson qualified as A
import Data.Version
import Data.Foldable (forM_)
import Text.PrettyPrint.Boxes qualified as Boxes
import System.Directory (listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import Language.PureScript.Docs
import Language.PureScript.Publish (PublishOptions(..), defaultPublishOptions)
import Language.PureScript.Publish qualified as Publish
import Language.PureScript.Publish.ErrorsWarnings qualified as Publish

import Test.Hspec
import TestUtils hiding (inferForeignModules, makeActions)

spec :: Spec
spec = do
  context "preparePackage with json roundtrips" $ do
    it "purescript-prelude" $ do
      testPackage
        "tests/support/bower_components/purescript-prelude"
        "bower.json"
        "../../prelude-resolutions.json"

    it "basic example (bower.json)" $ do
      testPackage
        "tests/purs/publish/basic-example"
        "bower.json"
        "resolutions.json"

    it "basic example (purs.json)" $ do
      testPackage
        "tests/purs/publish/basic-example"
        "purs.json"
        "resolutions.json"

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
  let before' = A.encode pkg
  in case A.eitherDecode before' of
       Left err -> ParseFailed err
       Right parsed -> do
         let after' = A.encode (parsed :: UploadedPackage)
         if before' == after'
           then Pass before'
           else Mismatch before' after'

testRunOptions :: FilePath -> FilePath -> PublishOptions
testRunOptions manifestFile resolutionsFile = defaultPublishOptions
  { publishResolutionsFile = resolutionsFile
  , publishManifestFile = manifestFile
  , publishGetVersion = return testVersion
  , publishGetTagTime = const (liftIO getCurrentTime)
  , publishWorkingTreeDirty = return ()
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

-- | Given a directory which contains a package, produce JSON from it, and then
-- | attempt to parse it again, and ensure that it doesn't change.
testPackage :: FilePath -> FilePath -> FilePath -> Expectation
testPackage packageDir manifestFile resolutionsFile = do
  res <- preparePackage packageDir manifestFile resolutionsFile
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
preparePackage :: FilePath -> FilePath -> FilePath -> IO (Either Publish.PackageError UploadedPackage)
preparePackage packageDir manifestFile resolutionsFile =
  pushd packageDir $ do
    removeDirectoryRecursiveIfPresent "output"
    Publish.preparePackage (testRunOptions manifestFile resolutionsFile)

removeDirectoryRecursiveIfPresent :: FilePath -> IO ()
removeDirectoryRecursiveIfPresent =
  void . tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive
