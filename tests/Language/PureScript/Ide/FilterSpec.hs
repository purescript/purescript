{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.FilterSpec where

import           Protolude
import           Data.List.NonEmpty
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Test as T
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

moduleA, moduleB, moduleC, moduleD :: Module
moduleA = (P.moduleNameFromString "Module.A", [T.ideValue "function1" Nothing])
moduleB = (P.moduleNameFromString "Module.B", [T.ideValue "data1" Nothing])
moduleC = (P.moduleNameFromString "Module.C", [T.ideType "List" Nothing])
moduleD = (P.moduleNameFromString "Module.D", [T.ideKind "kind1"])

modules :: [Module]
modules = [moduleA, moduleB]

runEq :: Text -> [Module]
runEq s = applyFilters [equalityFilter s] modules

runPrefix :: Text -> [Module]
runPrefix s = applyFilters [prefixFilter s] modules

runModule :: [P.ModuleName] -> [Module]
runModule ms = applyFilters [moduleFilter ms] modules

runNamespace :: NonEmpty IdeNamespace -> [Module] -> [Module]
runNamespace namespaces = applyFilters [namespaceFilter namespaces]

spec :: Spec
spec = do
  describe "equality Filter" $ do
    it "removes empty modules" $
      runEq "test" `shouldBe` []
    it "keeps function declarations that are equal" $
      runEq "function1" `shouldBe` [moduleA]
    it "keeps data declarations that are equal" $
      runEq "data1" `shouldBe` [moduleB]
  describe "prefixFilter" $ do
    it "keeps everything on empty string" $
      runPrefix "" `shouldBe` modules
    it "keeps functionname prefix matches" $
      runPrefix "fun" `shouldBe` [moduleA]
    it "keeps data decls prefix matches" $
      runPrefix "dat" `shouldBe` [moduleB]
  describe "moduleFilter" $ do
    it "removes everything on empty input" $
      runModule [] `shouldBe` []
    it "only keeps the specified modules" $
      runModule [P.moduleNameFromString "Module.A"] `shouldBe` [moduleA]
    it "ignores modules that are not in scope" $
      runModule (P.moduleNameFromString <$> ["Module.A", "Unknown"]) `shouldBe` [moduleA]
  describe "namespaceFilter" $ do
    it "extracts modules by filtering `value` namespaces" $
      runNamespace (fromList [IdeNSValue])
        [moduleA, moduleB, moduleD] `shouldBe` [moduleA, moduleB]
    it "extracts no modules by filtering `value` namespaces" $
      runNamespace (fromList [IdeNSValue])
        [moduleD] `shouldBe` []
    it "extracts modules by filtering `type` namespaces" $
      runNamespace (fromList [IdeNSType])
        [moduleA, moduleB, moduleC] `shouldBe` [moduleC]
    it "extracts no modules by filtering `type` namespaces" $
      runNamespace (fromList [IdeNSType])
        [moduleA, moduleB] `shouldBe` []
    it "extracts modules by filtering `kind` namespaces" $
      runNamespace (fromList [IdeNSKind])
        [moduleA, moduleB, moduleD] `shouldBe` [moduleD]
    it "extracts no modules by filtering `kind` namespaces" $
      runNamespace (fromList [IdeNSKind])
        [moduleA, moduleB] `shouldBe` []
    it "extracts modules by filtering `value` and `type` namespaces" $
      runNamespace (fromList [ IdeNSValue, IdeNSType])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleA, moduleB, moduleC]
    it "extracts modules by filtering `value` and `kind` namespaces" $
      runNamespace (fromList [ IdeNSValue, IdeNSKind])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleA, moduleB, moduleD]
    it "extracts modules by filtering `type` and `kind` namespaces" $
      runNamespace (fromList [ IdeNSType, IdeNSKind])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleC, moduleD]
    it "extracts modules by filtering `value`, `type` and `kind` namespaces" $
      runNamespace (fromList [ IdeNSValue, IdeNSType, IdeNSKind])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleA, moduleB, moduleC, moduleD]
