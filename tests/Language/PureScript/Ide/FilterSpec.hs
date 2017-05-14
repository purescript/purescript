{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.FilterSpec where

import           Protolude
import           Data.List.NonEmpty as L
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Test as T
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

value :: Text -> IdeDeclarationAnn
value s = IdeDeclarationAnn emptyAnn (IdeDeclValue (IdeValue (P.Ident (toS s)) P.REmpty))

type' :: Text -> IdeDeclarationAnn
type' s = IdeDeclarationAnn emptyAnn (IdeDeclType (IdeType (P.ProperName (toS s)) P.kindType))

moduleA, moduleB, moduleC, moduleD :: Module
moduleA = (P.moduleNameFromString "Module.A", [value "function1"])
moduleB = (P.moduleNameFromString "Module.B", [value "data1"])
moduleC = (P.moduleNameFromString "Module.C", [type' "List"])
moduleD = (P.moduleNameFromString "Module.D", [T.ideKind "kind1"])

modules :: [Module]
modules = [moduleA, moduleB]

runEq :: Text -> [Module]
runEq s = applyFilters [equalityFilter s] modules

runPrefix :: Text -> [Module]
runPrefix s = applyFilters [prefixFilter s] modules

runModule :: [P.ModuleName] -> [Module]
runModule ms = applyFilters [moduleFilter ms] modules

runNamespace :: L.NonEmpty IdeNamespace ->  [Module] -> [Module]
runNamespace nss = applyFilters [namespaceFilter nss]

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
  describe "namespaceFilter" $ do
    it "extracts no modules by filtering invalid (empty) namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString ""])
        [moduleA, moduleB, moduleD] `shouldBe` []
    it "extracts modules by filtering `value` namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString "value"])
        [moduleA, moduleB, moduleD] `shouldBe` [moduleA, moduleB]
    it "extracts no modules by filtering `value` namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString "value"])
        [moduleD] `shouldBe` []
    it "extracts modules by filtering `type` namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString "type"])
        [moduleA, moduleB, moduleC] `shouldBe` [moduleC]
    it "extracts no modules by filtering `type` namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString "type"])
        [moduleA, moduleB] `shouldBe` []
    it "extracts modules by filtering `kind` namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString "kind"])
        [moduleA, moduleB, moduleD] `shouldBe` [moduleD]
    it "extracts no modules by filtering `kind` namespaces" $
      runNamespace (L.fromList [ideNamespaceFromString "kind"])
        [moduleA, moduleB] `shouldBe` []
    it "extracts modules by filtering `value`, `type` and `kind` namespaces" $
      runNamespace (L.fromList
                      [ ideNamespaceFromString "value"
                      , ideNamespaceFromString "type"
                      , ideNamespaceFromString "kind"])
                        [moduleA, moduleB, moduleC, moduleD] `shouldBe` [moduleA, moduleB, moduleC, moduleD]
  describe "moduleFilter" $ do
    it "removes everything on empty input" $
      runModule [] `shouldBe` []
    it "only keeps the specified modules" $
      runModule [P.moduleNameFromString "Module.A"] `shouldBe` [moduleA]
    it "ignores modules that are not in scope" $
      runModule (P.moduleNameFromString <$> ["Module.A", "Unknown"]) `shouldBe` [moduleA]
