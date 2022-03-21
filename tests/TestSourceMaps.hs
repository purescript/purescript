module TestSourceMaps where

import Prelude

import Control.Monad (forM_)
import Test.Hspec
import System.FilePath (replaceExtension, (</>), takeFileName)
import qualified Language.PureScript as P
import qualified Data.ByteString as BS
import qualified Language.PureScript.CST as CST
import System.IO.UTF8 (readUTF8FileT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import TestUtils (inferForeignModules, makeActions, goldenVsString, modulesDir, getTestFiles, SupportModules (..))
import Data.Foldable (for_)
import qualified Data.Set as Set
import qualified Data.Map as M

spec :: SpecWith SupportModules
spec =
  goldenFiles

goldenFiles :: SpecWith SupportModules
goldenFiles = do
  sourceMapsFiles <- runIO $ getTestFiles "sourcemaps"

  describe "golden files" $
    forM_ sourceMapsFiles $ \dir ->
      for_ dir $ \file -> 
        it ("'" <> takeFileName file <> "' should compile to expected output") $ \support ->
          assertCompilesToExpectedOutput support file

compile
  :: SupportModules
  -> FilePath
  -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
compile SupportModules{..} inputFile = P.runMake compilationOptions $ do
  text <- liftIO $ readUTF8FileT inputFile

  smModule <- CST.unwrapParserError inputFile . snd . CST.parseFromFile inputFile $ text
  foreigns <- inferForeignModules [(inputFile, smModule)]
  let
    actions = makeActions supportModules (foreigns `M.union` supportForeigns)
  pure <$> P.rebuildModule actions supportExterns smModule

  where

  compilationOptions :: P.Options
  compilationOptions = P.defaultOptions { P.optionsCodegenTargets = Set.fromList [P.JS, P.JSSourceMap] }

assertCompilesToExpectedOutput
  :: SupportModules
  -> FilePath
  -> Expectation
assertCompilesToExpectedOutput supportModules inputFile = do
  (result, _) <- compile supportModules inputFile
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
    Right _ ->
      goldenVsString
        (replaceExtension inputFile ".out.js.map")
        (BS.readFile $ modulesDir </> "Main/index.js.map")
