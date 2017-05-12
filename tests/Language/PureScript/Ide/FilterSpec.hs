{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.FilterSpec where

import           Protolude
import           Data.List.NonEmpty
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

value :: Text -> IdeDeclarationAnn
value s = IdeDeclarationAnn emptyAnn (IdeDeclValue (IdeValue (P.Ident (toS s)) P.REmpty))

kind :: Text -> IdeDeclarationAnn
kind s = IdeDeclarationAnn emptyAnn (IdeDeclKind (P.ProperName s))

moduleA, moduleB :: Module
moduleA = (P.moduleNameFromString "Module.A", [value "function1"])
moduleB = (P.moduleNameFromString "Module.B", [kind "kind1"])

modules :: [Module]
modules = [moduleA, moduleB]

runEq :: Text -> [Module]
runEq s = applyFilters [equalityFilter s] modules

runPrefix :: Text -> [Module]
runPrefix s = applyFilters [prefixFilter s] modules

runModule :: [P.ModuleName] -> [Module]
runModule ms = applyFilters [moduleFilter ms] modules

runNamespace :: NonEmpty IdeNamespace -> [Module]
runNamespace namespaces = applyFilters [namespaceFilter namespaces] modules

spec :: Spec
spec = do
  describe "equality Filter" $ do
    it "removes empty modules" $
      runEq "test" `shouldBe` []
    it "keeps function declarations that are equal" $
      runEq "function1" `shouldBe` [moduleA]
    it "keeps data declarations that are equal" $
      runEq "kind1" `shouldBe` [moduleB]
  describe "prefixFilter" $ do
    it "keeps everything on empty string" $
      runPrefix "" `shouldBe` modules
    it "keeps functionname prefix matches" $
      runPrefix "fun" `shouldBe` [moduleA]
    it "keeps data decls prefix matches" $
      runPrefix "kin" `shouldBe` [moduleB]
  describe "moduleFilter" $ do
    it "removes everything on empty input" $
      runModule [] `shouldBe` []
    it "only keeps the specified modules" $
      runModule [P.moduleNameFromString "Module.A"] `shouldBe` [moduleA]
    it "ignores modules that are not in scope" $
      runModule (P.moduleNameFromString <$> ["Module.A", "Unknown"]) `shouldBe` [moduleA]
  describe "namespaceFilter" $ do
    it "keeps only values" $
      runNamespace (fromList [IdeNSValue]) `shouldBe` [moduleA]
    it "keeps both types and kinds" $
      runNamespace (fromList [IdeNSType, IdeNSKind]) `shouldBe` [moduleB]
    it "keeps values, types and kinds" $
      runNamespace (fromList [IdeNSValue, IdeNSKind]) `shouldBe` [moduleA, moduleB]
