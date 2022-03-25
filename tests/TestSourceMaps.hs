module TestSourceMaps where

import Prelude

import Control.Monad (forM_)
import Test.Hspec
import System.FilePath (replaceExtension, (</>), takeFileName)
import qualified Language.PureScript as P
import qualified Data.ByteString as BS
import TestUtils (goldenVsString, modulesDir, getTestFiles, SupportModules (..), compile')
import qualified Data.Set as Set
import TestCompiler (getTestMain)

spec :: SpecWith SupportModules
spec =
  goldenFiles

goldenFiles :: SpecWith SupportModules
goldenFiles = do
  sourceMapsFiles <- runIO $ getTestFiles "sourcemaps"

  describe "golden files" $
    forM_ sourceMapsFiles $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile to expected output") $ \support ->
        assertCompilesToExpectedOutput support testPurs

compilationOptions :: P.Options
compilationOptions = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.JSSourceMap] }

assertCompilesToExpectedOutput
  :: SupportModules
  -> [FilePath]
  -> Expectation
assertCompilesToExpectedOutput support inputFiles = do
  (result, _) <- compile' compilationOptions False support inputFiles
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
    Right _ -> do
      let
        modulePath = getTestMain inputFiles
        strLength = length :: String -> Int
        dirPrefix = strLength "tests/purs/sourcemaps/"
        moduleSuffix = do
          let modulePlusExt = drop dirPrefix modulePath
          take (strLength modulePlusExt - strLength ".purs") modulePlusExt
      goldenVsString
        (replaceExtension modulePath ".out.js.map")
        (BS.readFile $ modulesDir </> "SourceMaps." <> moduleSuffix <> "/index.js.map")