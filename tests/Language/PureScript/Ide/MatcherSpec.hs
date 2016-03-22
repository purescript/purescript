{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.MatcherSpec where

import           Data.Text                       (Text)
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Integration
import           Test.Hspec

completions :: [Completion]
completions = [
  Completion ("", "firstResult", ""),
  Completion ("", "secondResult", ""),
  Completion ("", "fiult", "")
  ]

mkResult :: [Int] -> [Completion]
mkResult = map (completions !!)

runFlex :: Text -> [Completion]
runFlex s = runMatcher (flexMatcher s) completions

setup :: IO ()
setup = do
  deleteOutputFolder
  _ <- compileTestProject
  _ <- startServer
  _ <- loadModuleWithDeps "Main"
  return ()

teardown :: IO ()
teardown = quitServer

spec :: Spec
spec = do
  describe "Flex Matcher" $ do
    it "doesn't match on an empty string" $
       runFlex "" `shouldBe` []
    it "matches on equality" $
      runFlex "firstResult" `shouldBe` mkResult [0]
    it "scores short matches higher and sorts accordingly" $
      runFlex "filt" `shouldBe` mkResult [2, 0]

  beforeAll_ setup $ afterAll_ teardown $
    describe "Integration Tests: Flex Matcher" $ do
      it "doesn't match on an empty string" $ do
        cs <- getFlexCompletions ""
        cs `shouldBe` []
      it "matches on equality" $ do
        cs <- getFlexCompletions "const"
        cs `shouldBe` [("Main", "const", "forall a b. a -> b -> a")]
