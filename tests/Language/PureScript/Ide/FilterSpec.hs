{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.FilterSpec where

import           Data.Text                      (Text)
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

value :: Text -> IdeDeclaration
value s = IdeValue s $ P.TypeWildcard $ P.SourceSpan "" (P.SourcePos 0 0) (P.SourcePos 0 0)

modules :: [Module]
modules =
  [ (P.moduleNameFromString "Module.A", [value "function1"])
  , (P.moduleNameFromString "Module.B", [value "data1"])
  ]

runEq :: Text -> [Module]
runEq s = runFilter (equalityFilter s) modules

runPrefix :: Text -> [Module]
runPrefix s = runFilter (prefixFilter s) modules

runModule :: [P.ModuleName] -> [Module]
runModule ms = runFilter (moduleFilter ms) modules

spec :: Spec
spec = do
  describe "equality Filter" $ do
    it "removes empty modules" $
      runEq "test" `shouldBe` []
    it "keeps function declarations that are equal" $
      runEq "function1" `shouldBe` [head modules]
    it "keeps data declarations that are equal" $
      runEq "data1" `shouldBe` [modules !! 1]
  describe "prefixFilter" $ do
    it "keeps everything on empty string" $
      runPrefix "" `shouldBe` modules
    it "keeps functionname prefix matches" $
      runPrefix "fun" `shouldBe` [head modules]
    it "keeps data decls prefix matches" $
      runPrefix "dat" `shouldBe` [modules !! 1]
  describe "moduleFilter" $ do
    it "removes everything on empty input" $
      runModule [] `shouldBe` []
    it "only keeps the specified modules" $
      runModule [P.moduleNameFromString "Module.A"] `shouldBe` [head modules]
    it "ignores modules that are not in scope" $
      runModule (P.moduleNameFromString <$> ["Module.A", "Unknown"]) `shouldBe` [head modules]
