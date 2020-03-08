{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.RebuildSpec where

import           Protolude

import qualified Data.Set as Set
import qualified Language.PureScript as P
import           Language.PureScript.AST.SourcePos (spanName)
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Ide.Test as Test
import           System.FilePath
import           System.Directory (doesFileExist, removePathForcibly)
import           Test.Hspec

defaultTarget :: Set P.CodegenTarget
defaultTarget = Set.singleton P.JS

load :: [Text] -> Command
load = LoadSync . map Test.mn

rebuild :: FilePath -> Command
rebuild fp = Rebuild ("src" </> fp) Nothing defaultTarget

rebuildSync :: FilePath -> Command
rebuildSync fp = RebuildSync ("src" </> fp) Nothing defaultTarget

spec :: Spec
spec = describe "Rebuilding single modules" $ do
    it "rebuilds a correct module without dependencies successfully" $ do
      ([_, result], _) <- Test.inProject $
        Test.runIde [ load ["RebuildSpecSingleModule"]
                    , rebuild "RebuildSpecSingleModule.purs"
                    ]
      result `shouldSatisfy` isRight
    it "fails to rebuild an incorrect module without dependencies and returns the errors" $ do
      ([result], _) <- Test.inProject $
        Test.runIde [ rebuild "RebuildSpecSingleModule.fail" ]
      result `shouldSatisfy` isLeft
    it "rebuilds a correct module with its dependencies successfully" $ do
      ([_, result], _) <- Test.inProject $
        Test.runIde [ load ["RebuildSpecWithDeps", "RebuildSpecDep"]
                    , rebuild "RebuildSpecWithDeps.purs"
                    ]
      result `shouldSatisfy` isRight
    it "rebuilds a correct module that has reverse dependencies" $ do
      ([_, result], _) <- Test.inProject $
        Test.runIde [ load ["RebuildSpecWithDeps"], rebuild "RebuildSpecDep.purs" ]
      result `shouldSatisfy` isRight
    it "fails to rebuild a module if its dependencies are not loaded" $ do
      ([_, result], _) <- Test.inProject $
        Test.runIde [ load ["RebuildSpecWithDeps"], rebuild "RebuildSpecWithDeps.purs" ]
      result `shouldSatisfy` isLeft
    it "rebuilds a correct module with a foreign file" $ do
      ([_, result], _) <- Test.inProject $
        Test.runIde [ load ["RebuildSpecWithForeign"], rebuild "RebuildSpecWithForeign.purs" ]
      result `shouldSatisfy` isRight
    it "fails to rebuild a module with a foreign import but no file" $ do
      ([result], _) <- Test.inProject $
        Test.runIde [ rebuild "RebuildSpecWithMissingForeign.fail" ]
      result `shouldSatisfy` isLeft
    it "completes a hidden identifier after rebuilding" $ do
      ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
        Test.runIde [ rebuildSync "RebuildSpecWithHiddenIdent.purs"
                    , Complete [] (flexMatcher "hid") (Just (Test.mn "RebuildSpecWithHiddenIdent")) defaultCompletionOptions]
      complIdentifier result `shouldBe` "hidden"
    it "uses the specified `actualFile` for location information" $ do
      ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
        Test.runIde'
          Test.defConfig
          emptyIdeState
          [ RebuildSync ("src" </> "RebuildSpecWithHiddenIdent.purs") (Just "actualFile") defaultTarget
          , Complete [] (flexMatcher "hid") (Just (Test.mn "RebuildSpecWithHiddenIdent")) defaultCompletionOptions]
      map spanName (complLocation result) `shouldBe` Just "actualFile"
    it "doesn't produce JS when an empty target list is supplied" $ do
      exists <- Test.inProject $ do
        let indexJs = "output" </> "RebuildSpecSingleModule" </> "index.js"
        removePathForcibly ("output" </> "RebuildSpecSingleModule")
        _ <- Test.runIde [ RebuildSync ("src" </> "RebuildSpecSingleModule.purs") Nothing Set.empty ]
        doesFileExist indexJs
      exists `shouldBe` False
    it "does produce corefn if it's a codegen target" $ do
      exists <- Test.inProject $ do
        let corefn = "output" </> "RebuildSpecSingleModule" </> "corefn.json"
        removePathForcibly ("output" </> "RebuildSpecSingleModule")
        _ <- Test.runIde [ RebuildSync ("src" </> "RebuildSpecSingleModule.purs") Nothing (Set.singleton P.CoreFn) ]
        doesFileExist corefn
      exists `shouldBe` True
