module TestInteractive where

import Prelude

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Data.List.NonEmpty qualified as NEL
import Data.List (nub)

import Language.PureScript.Interactive.Directive
  ( directiveStrings
  , directiveStrings'
  , stringsFor
  , stringFor
  , directivesFor
  , directivesFor'
  , help
  )
import Language.PureScript.Interactive.Types (Directive(..))

spec :: Spec
spec = do
  describe "Interactive.Directive" $ do
    directiveStringsTests
    directiveStrings'Tests
    stringsForTests
    stringForTests
    directivesFor'Tests
    directivesForTests
    helpTests

directiveStringsTests :: Spec
directiveStringsTests = describe "directiveStrings" $ do
  it "should have non-empty string lists for each directive" $ do
    let allHaveElements = all (not . null . NEL.toList . snd) directiveStrings
    allHaveElements `shouldBe` True

directiveStrings'Tests :: Spec
directiveStrings'Tests = describe "directiveStrings'" $ do
  it "should be a flattened version of directiveStrings" $ do
    let expectedLength = sum (length . NEL.toList . snd <$> directiveStrings)
    length directiveStrings' `shouldBe` expectedLength

  it "should contain appropriate directives" $ do
    lookup "help" directiveStrings' `shouldBe` Just Help
    lookup "?" directiveStrings' `shouldBe` Just Help
    lookup "quit" directiveStrings' `shouldBe` Just Quit
    lookup "reload" directiveStrings' `shouldBe` Just Reload

stringsForTests :: Spec
stringsForTests = describe "stringsFor" $ do
  it "should return all strings for a directive" $ do
    NEL.toList (stringsFor Help) `shouldBe` ["?", "help"]
    NEL.toList (stringsFor Quit) `shouldBe` ["quit"]
    NEL.toList (stringsFor Reload) `shouldBe` ["reload"]

stringForTests :: Spec
stringForTests = describe "stringFor" $ do
  it "should return the first string for a directive" $ do
    stringFor Help `shouldBe` "?"
    stringFor Quit `shouldBe` "quit"
    stringFor Reload `shouldBe` "reload"

directivesFor'Tests :: Spec
directivesFor'Tests = describe "directivesFor'" $ do
  it "should return matching directives and their string representations" $ do
    directivesFor' "h" `shouldBe` [(Help, "help")]
    directivesFor' "he" `shouldBe` [(Help, "help")]
    directivesFor' "?" `shouldBe` [(Help, "?")]
    directivesFor' "q" `shouldBe` [(Quit, "quit")]

  it "should handle ambiguous prefixes" $ do
    directivesFor' "" `shouldSatisfy` (not . null)
    length (directivesFor' "") `shouldBe` length directiveStrings'

  it "should return empty list for non-matching prefixes" $ do
    directivesFor' "xyz" `shouldBe` []

directivesForTests :: Spec
directivesForTests = describe "directivesFor" $ do
  it "should return just the directive part" $ do
    directivesFor "h" `shouldBe` [Help]
    directivesFor "q" `shouldBe` [Quit]
    directivesFor "xyz" `shouldBe` []

helpTests :: Spec
helpTests = describe "help" $ do
  it "should contain help for all directives" $ do
    let helpDirectives = map (\(d, _, _) -> d) help
    length (nub helpDirectives) `shouldBe` length directiveStrings

  it "should contain descriptive help text" $ do
    let helpTexts = map (\(_, _, text) -> text) help
    all (not . null) helpTexts `shouldBe` True

  it "should include parameters where needed" $ do
    lookup Browse (map (\(d, a, _) -> (d, a)) help) `shouldBe` Just "<module>"
    lookup Type (map (\(d, a, _) -> (d, a)) help) `shouldBe` Just "<expr>"
