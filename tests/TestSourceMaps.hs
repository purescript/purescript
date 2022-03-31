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

-- Test module names must be prefixed with "SourceMaps." and reside under tests/purs/sourcemaps. 
--  eg. Module file tests/purs/sourcemaps/Test123.purs must be named SourceMaps.Test123  
-- Modules created for the purpose of testing specific github issues should be named SourceMaps.Bug<issue_number>. 
--  eg. File name tests/purs/sourcemaps/Bug123.purs, module name SourceMaps.Bug123
--
-- Test modules and compilation output can be conveniently copied to .source-maps/<module_name>/
--  by running the get-source-maps.sh script.
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
