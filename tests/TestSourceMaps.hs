module TestSourceMaps where

import Prelude

import Control.Monad (forM_)
import Test.Hspec
import System.FilePath (replaceExtension, takeFileName)
import qualified Language.PureScript as P
import qualified Data.ByteString as BS
import TestUtils (goldenVsString, getTestFiles, SupportModules (..), compile', ExpectedModuleName (IsSourceMap))
import qualified Data.Set as Set
import TestCompiler (getTestMain)

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
    forM_ sourceMapsFiles $ \inputFiles ->
      it ("'" <> takeFileName (getTestMain inputFiles) <> "' should compile to expected output") $ \support ->
        assertCompilesToExpectedOutput support inputFiles

assertCompilesToExpectedOutput
  :: SupportModules
  -> [FilePath]
  -> Expectation
assertCompilesToExpectedOutput support inputFiles = do

  let
    modulePath = getTestMain inputFiles

  (result, _) <- compile' compilationOptions (Just (IsSourceMap modulePath)) support inputFiles
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
    Right compiledModulePath ->
      goldenVsString
        (replaceExtension modulePath ".out.js.map")
        (BS.readFile compiledModulePath)

  where
    compilationOptions :: P.Options
    compilationOptions = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.JSSourceMap] }
