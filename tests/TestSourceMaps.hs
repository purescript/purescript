module TestSourceMaps where

import Prelude

import Control.Monad (void, forM_)
import Data.Aeson as Json
import Test.Hspec
import System.FilePath (replaceExtension, takeFileName, (</>), (<.>))
import qualified Language.PureScript as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import TestUtils (getTestFiles, SupportModules (..), compile', ExpectedModuleName (IsSourceMap))
import qualified Data.Set as Set
import TestCompiler (getTestMain)
import System.Process.Typed (proc, readProcess_)

spec :: SpecWith SupportModules
spec =
  goldenFiles

-- See the CONTRIBUTING.md file for why the below requirements are needed.
-- Test files and their module names must abide by the following requirements:
-- - Test files must reside in the @tests/purs/sourcemaps/@ directory
-- - Module names must be prefixed with "SourceMaps." with the remainder
--   matching the file name. For example:
--    - File Name:   @tests/purs/sourcemaps/Test123.purs@
--    - Module Name: @SourceMaps.Test123@
--    - File Name:   @tests/purs/sourcemaps/Bug1234.purs@
--    - Module Name: @SourceMaps.Bug1234@
goldenFiles :: SpecWith SupportModules
goldenFiles = do
  sourceMapsFiles <- runIO $ getTestFiles "sourcemaps"

  describe "golden files" $
    forM_ sourceMapsFiles $ \inputFiles -> do
      let
        testName = fold
          [ "'"
          , takeFileName (getTestMain inputFiles)
          , "' should compile to expected output and produce a valid source map file"
          ]
      it testName $ \support -> do
        assertCompilesToExpectedValidOutput support inputFiles

assertCompilesToExpectedValidOutput
  :: SupportModules
  -> [FilePath]
  -> Expectation
assertCompilesToExpectedValidOutput support inputFiles = do

  let
    modulePath = getTestMain inputFiles

  (fileContents, (result, _)) <- compile' compilationOptions (Just (IsSourceMap modulePath)) support inputFiles
  let errorOptions = P.defaultPPEOptions { P.ppeFileContents = fileContents }
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors errorOptions $ errs
    Right actualSourceMapFile -> do
      let
        readAndDecode :: FilePath ->  IO (Maybe Json.Value)
        readAndDecode = fmap (Json.decode . LBS.fromStrict) . BS.readFile
      goldenFile <- readAndDecode $ replaceExtension modulePath ".out.js.map"
      actualFile <- readAndDecode actualSourceMapFile
      goldenFile `shouldBe` actualFile
      sourceMapIsValid actualSourceMapFile

  where
    compilationOptions :: P.Options
    compilationOptions = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.JSSourceMap] }

-- | Fails the test if the produced source maps are not valid.
sourceMapIsValid :: FilePath -> Expectation
sourceMapIsValid sourceMapFilePath = do
  let
    scriptPath = "tests" </> "support" </> "checkSourceMapValidity" <.> "js"
  void $ readProcess_ (proc "node" [scriptPath, sourceMapFilePath])
