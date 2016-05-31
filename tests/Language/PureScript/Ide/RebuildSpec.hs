module Language.PureScript.Ide.RebuildSpec where

import qualified Language.PureScript.Ide.Integration as Integration
import           System.FilePath
import           Test.Hspec

shouldBeSuccess :: String -> IO ()
shouldBeSuccess = shouldBe True . Integration.resultIsSuccess

shouldBeFailure :: String -> IO ()
shouldBeFailure = shouldBe False . Integration.resultIsSuccess

spec :: Spec
spec = before_ Integration.reset . describe "Rebuilding single modules" $ do
    it "rebuilds a correct module without dependencies successfully" $ do
      _ <- Integration.loadModuleWithDeps "RebuildSpecSingleModule"
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecSingleModule.purs"
      Integration.rebuildModule file >>= shouldBeSuccess
    it "fails to rebuild an incorrect module without dependencies and returns the errors" $ do
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecSingleModule.fail"
      Integration.rebuildModule file >>= shouldBeFailure
    it "rebuilds a correct module with its dependencies successfully" $ do
      _ <- Integration.loadModuleWithDeps "RebuildSpecWithDeps"
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecWithDeps.purs"
      Integration.rebuildModule file >>= shouldBeSuccess
    it "rebuilds a correct module that has reverse dependencies" $ do
      _ <- Integration.loadModuleWithDeps "RebuildSpecWithDeps"
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecDep.purs"
      Integration.rebuildModule file >>= shouldBeSuccess
    it "fails to rebuild a module if its dependencies are not loaded" $ do
      _ <- Integration.loadModule "RebuildSpecWithDeps"
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecWithDeps.purs"
      Integration.rebuildModule file >>= shouldBeFailure
    it "rebuilds a correct module with a foreign file" $ do
      _ <- Integration.loadModuleWithDeps "RebuildSpecWithForeign"
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecWithForeign.purs"
      Integration.rebuildModule file >>= shouldBeSuccess
    it "fails to rebuild a module with a foreign import but no file" $ do
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecWithMissingForeign.fail"
      Integration.rebuildModule file >>= shouldBeFailure
    it "completes a hidden identifier after rebuilding" $ do
      pdir <- Integration.projectDirectory
      let file = pdir </> "src" </> "RebuildSpecWithHiddenIdent.purs"
      Integration.rebuildModule file >>= shouldBeSuccess
      res <- Integration.getFlexCompletionsInModule "hid" "RebuildSpecWithHiddenIdent"
      shouldBe False (null res)
