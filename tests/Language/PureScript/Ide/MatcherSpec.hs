module Language.PureScript.Ide.MatcherSpec where

import Protolude

import Language.PureScript qualified as P
import Language.PureScript.Ide.Matcher (flexMatcher, runMatcher)
import Language.PureScript.Ide.Types (IdeDeclaration(..), IdeDeclarationAnn, IdeValue(..), Match(..))
import Language.PureScript.Ide.Util (withEmptyAnn)
import Test.Hspec (Spec, describe, it, shouldBe)

value :: Text -> IdeDeclarationAnn
value s = withEmptyAnn (IdeDeclValue (IdeValue (P.Ident (toS s)) P.srcREmpty))

firstResult, secondResult, fiult :: Match IdeDeclarationAnn
firstResult = Match (P.moduleNameFromString "Match", value "firstResult")
secondResult = Match (P.moduleNameFromString "Match", value "secondResult")
fiult = Match (P.moduleNameFromString "Match", value "fiult")

completions :: [Match IdeDeclarationAnn]
completions = [firstResult, secondResult, fiult]

runFlex :: Text -> [Match IdeDeclarationAnn]
runFlex s = runMatcher (flexMatcher s) completions

spec :: Spec
spec = do
  describe "Flex Matcher" $ do
    it "doesn't match on an empty string" $
       runFlex "" `shouldBe` []
    it "matches on equality" $
      runFlex "firstResult" `shouldBe` [firstResult]
    it "scores short matches higher and sorts accordingly" $
      runFlex "filt" `shouldBe` [fiult, firstResult]
