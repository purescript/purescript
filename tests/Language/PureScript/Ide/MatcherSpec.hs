{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.MatcherSpec where

import           Control.Monad                       (void)
import           Data.Text                           (Text)
import qualified Language.PureScript                 as P
import           Language.PureScript.Ide.Integration
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Test.Hspec

value :: Text -> ExternDecl
value s = ValueDeclaration s $ P.TypeWildcard $ P.SourceSpan "" (P.SourcePos 0 0) (P.SourcePos 0 0)

completions :: [Match]
completions =
  [ Match "" (value "firstResult")
  , Match "" (value "secondResult")
  , Match "" (value "fiult")
  ]

mkResult :: [Int] -> [Match]
mkResult = map (completions !!)

runFlex :: Text -> [Match]
runFlex s = runMatcher (flexMatcher s) completions

setup :: IO ()
setup = reset *> void (loadModuleWithDeps "Main")

spec :: Spec
spec = do
  describe "Flex Matcher" $ do
    it "doesn't match on an empty string" $
       runFlex "" `shouldBe` []
    it "matches on equality" $
      runFlex "firstResult" `shouldBe` mkResult [0]
    it "scores short matches higher and sorts accordingly" $
      runFlex "filt" `shouldBe` mkResult [2, 0]

  beforeAll_ setup . describe "Integration Tests: Flex Matcher" $ do
      it "doesn't match on an empty string" $ do
        cs <- getFlexCompletions ""
        cs `shouldBe` []
      it "matches on equality" $ do
        cs <- getFlexCompletions "const"
        cs `shouldBe` [("Main", "const", "forall a b. a -> b -> a")]
