{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.FilterSpec where

import           Protolude
import           Data.List.NonEmpty
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Filter.Declaration as D
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Test as T
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

moduleA, moduleB, moduleC, moduleD, moduleE, moduleF, moduleG, moduleH, moduleI :: Module
moduleA = (P.moduleNameFromString "Module.A", [T.ideValue "function1" Nothing])
moduleB = (P.moduleNameFromString "Module.B", [T.ideValue "data1" Nothing])
moduleC = (P.moduleNameFromString "Module.C", [T.ideType "List" Nothing []])
moduleD = (P.moduleNameFromString "Module.D", [T.ideKind "kind1"])
moduleE = (P.moduleNameFromString "Module.E", [T.ideSynonym "SFType" Nothing Nothing `annLoc` synonymSS])
moduleF = (P.moduleNameFromString "Module.F", [T.ideDtor "DtorA" "TypeA" Nothing])
moduleG = (P.moduleNameFromString "Module.G", [T.ideTypeClass "MyClass" P.kindType []])
moduleH = (P.moduleNameFromString "Module.H", [T.ideValueOp "<$>" (P.Qualified Nothing (Left "")) 0 Nothing Nothing])
moduleI = (P.moduleNameFromString "Module.I", [T.ideTypeOp "~>" (P.Qualified Nothing "") 0 Nothing Nothing])

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

runDeclaration :: [D.IdeDeclaration] -> [Module] -> [Module]
runDeclaration decls = applyFilters [declarationTypeFilter decls]

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
  describe "declarationTypeFilter" $ do
    let moduleADecl = D.IdeDeclaration D.Value
        moduleCDecl = D.IdeDeclaration D.Type
        moduleDDecl = D.IdeDeclaration D.Kind
        moduleEDecl = D.IdeDeclaration D.Synonym
        moduleFDecl = D.IdeDeclaration D.DataConstructor
        moduleGDecl = D.IdeDeclaration D.TypeClass
        moduleHDecl = D.IdeDeclaration D.ValueOperator
        moduleIDecl = D.IdeDeclaration D.TypeOperator
    it "keeps everything on empty list of declarations" $
      runDeclaration []
        [moduleA, moduleB, moduleD] `shouldBe` [moduleA, moduleB, moduleD]
    it "extracts modules by filtering `value` declarations" $
      runDeclaration [moduleADecl]
        [moduleA, moduleB, moduleD] `shouldBe` [moduleA, moduleB]
    it "removes everything if no `value` declarations has been found" $
      runDeclaration [moduleADecl]
        [moduleD, moduleG, moduleE, moduleH] `shouldBe` []
    it "extracts module by filtering `type` declarations" $
      runDeclaration [moduleCDecl]
        [moduleA, moduleB, moduleC, moduleD, moduleE] `shouldBe` [moduleC]
    it "removes everything if a `type` declaration have not been found" $
      runDeclaration [moduleCDecl]
        [moduleA, moduleG, moduleE, moduleH] `shouldBe` []
    it "extracts module by filtering `synonym` declarations" $
      runDeclaration [moduleEDecl]
        [moduleA, moduleB, moduleD, moduleE] `shouldBe` [moduleE]
    it "removes everything if a `synonym` declaration have not been found" $
      runDeclaration [moduleEDecl]
        [moduleA, moduleB, moduleC, moduleH] `shouldBe` []
    it "extracts module by filtering `constructor` declarations" $
      runDeclaration [moduleFDecl]
        [moduleA, moduleB, moduleC, moduleF] `shouldBe` [moduleF]
    it "removes everything if a `constructor` declaration have not been found" $
      runDeclaration [moduleFDecl]
        [moduleA, moduleB, moduleC, moduleH] `shouldBe` []
    it "extracts module by filtering `typeclass` declarations" $
      runDeclaration [moduleGDecl]
        [moduleA, moduleC, moduleG] `shouldBe` [moduleG]
    it "removes everything if a `typeclass` declaration have not been found" $
      runDeclaration [moduleGDecl]
        [moduleA, moduleB, moduleC, moduleH] `shouldBe` []
    it "extracts modules by filtering `valueoperator` declarations" $
      runDeclaration [moduleHDecl]
        [moduleA, moduleC, moduleG, moduleH, moduleF] `shouldBe` [moduleH]
    it "removes everything if a `valueoperator` declaration have not been found" $
      runDeclaration [moduleHDecl]
        [moduleA, moduleB, moduleC, moduleD] `shouldBe` []
    it "extracts modules by filtering `typeoperator` declarations" $
      runDeclaration [moduleIDecl]
        [moduleA, moduleC, moduleG, moduleI, moduleF] `shouldBe` [moduleI]
    it "removes everything if a `typeoperator` declaration have not been found" $
      runDeclaration [moduleIDecl]
        [moduleA, moduleD] `shouldBe` []
    it "extracts module by filtering `kind` declarations" $
      runDeclaration [moduleCDecl]
        [moduleA, moduleC, moduleG, moduleI, moduleF] `shouldBe` [moduleC]
    it "removes everything if a `kind` declaration have not been found" $
      runDeclaration [moduleCDecl]
        [moduleA, moduleD] `shouldBe` []
    it "extracts modules by filtering `value` and `synonym` declarations" $
      runDeclaration [moduleADecl, moduleEDecl]
        [moduleA, moduleB, moduleD, moduleE] `shouldBe` [moduleA, moduleB, moduleE]
    it "extracts modules by filtering `kind`, `synonym` and `valueoperator` declarations" $
      runDeclaration [moduleADecl, moduleDDecl, moduleHDecl]
        [moduleA, moduleB, moduleD, moduleG, moduleE, moduleH] `shouldBe` [moduleA, moduleB, moduleD, moduleH]
