{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.PureScript.Ide.MatcherSpec where

import           Protolude

import qualified Language.PureScript                 as P
import           Language.PureScript.Ide.Integration
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           Test.Hspec

value :: Text -> IdeDeclarationAnn
value s = withEmptyAnn (IdeDeclValue (IdeValue (P.Ident (toS s)) P.REmpty))

firstResult, secondResult, fiult :: Match IdeDeclarationAnn
firstResult = Match (P.moduleNameFromString "Match", value "firstResult")
secondResult = Match (P.moduleNameFromString "Match", value "secondResult")
fiult = Match (P.moduleNameFromString "Match", value "fiult")

completions :: [Match IdeDeclarationAnn]
completions = [firstResult, secondResult, fiult]

runFlex :: Text -> [Match IdeDeclarationAnn]
runFlex s = runMatcher (flexMatcher s) completions

setup :: IO ()
setup = reset *> void loadAll

spec :: Spec
spec = do
  describe "Flex Matcher" $ do
    it "doesn't match on an empty string" $
       runFlex "" `shouldBe` []
    it "matches on equality" $
      runFlex "firstResult" `shouldBe` [firstResult]
    it "scores short matches higher and sorts accordingly" $
      runFlex "filt" `shouldBe` [fiult, firstResult]

  beforeAll_ setup . describe "Integration Tests: Flex Matcher" $ do
      it "doesn't match on an empty string" $ do
        cs <- getFlexCompletions ""
        cs `shouldBe` []
      it "matches on equality" $ do
        -- ignore any position information
        (m, i, t, _) : _ <- getFlexCompletions "const"
        (m, i, t) `shouldBe` ("MatcherSpec", "const", "forall a b. a -> b -> a")
