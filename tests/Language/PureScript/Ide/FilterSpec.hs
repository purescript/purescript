{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.FilterSpec where

import           Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set
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

modules :: ModuleMap [IdeDeclarationAnn]
modules = Map.fromList [moduleA, moduleB]

runEq :: Text -> [Module]
runEq s = Map.toList (applyFilters [exactFilter s] modules)

runPrefix :: Text -> [Module]
runPrefix s = Map.toList $ applyFilters [prefixFilter s] modules

runModule :: [P.ModuleName] -> [Module]
runModule ms = Map.toList $ applyFilters [moduleFilter (Set.fromList ms)] modules

runNamespace :: Set IdeNamespace -> [Module] -> [Module]
runNamespace namespaces = Map.toList . applyFilters [namespaceFilter namespaces] . Map.fromList

runDeclaration :: [D.DeclarationType] -> [Module] -> [Module]
runDeclaration decls = Map.toList . applyFilters [declarationTypeFilter (Set.fromList decls)] . Map.fromList

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
      runPrefix "" `shouldBe` Map.toList modules
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
      runNamespace (Set.fromList [IdeNSValue])
        [moduleA, moduleB, moduleD] `shouldBe` [moduleA, moduleB]
    it "extracts no modules by filtering `value` namespaces" $
      runNamespace (Set.fromList [IdeNSValue])
        [moduleD] `shouldBe` []
    it "extracts modules by filtering `type` namespaces" $
      runNamespace (Set.fromList [IdeNSType])
        [moduleA, moduleB, moduleC] `shouldBe` [moduleC]
    it "extracts no modules by filtering `type` namespaces" $
      runNamespace (Set.fromList [IdeNSType])
        [moduleA, moduleB] `shouldBe` []
    it "extracts modules by filtering `kind` namespaces" $
      runNamespace (Set.fromList [IdeNSKind])
        [moduleA, moduleB, moduleD] `shouldBe` [moduleD]
    it "extracts no modules by filtering `kind` namespaces" $
      runNamespace (Set.fromList [IdeNSKind])
        [moduleA, moduleB] `shouldBe` []
    it "extracts modules by filtering `value` and `type` namespaces" $
      runNamespace (Set.fromList [ IdeNSValue, IdeNSType])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleA, moduleB, moduleC]
    it "extracts modules by filtering `value` and `kind` namespaces" $
      runNamespace (Set.fromList [ IdeNSValue, IdeNSKind])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleA, moduleB, moduleD]
    it "extracts modules by filtering `type` and `kind` namespaces" $
      runNamespace (Set.fromList [ IdeNSType, IdeNSKind])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleC, moduleD]
    it "extracts modules by filtering `value`, `type` and `kind` namespaces" $
      runNamespace (Set.fromList [ IdeNSValue, IdeNSType, IdeNSKind])
        [moduleA, moduleB, moduleC, moduleD]
        `shouldBe` [moduleA, moduleB, moduleC, moduleD]
  describe "declarationTypeFilter" $ do
    it "extracts modules by filtering `value` declarations" $
      runDeclaration [D.Value]
        [moduleA, moduleB, moduleD] `shouldBe` [moduleA, moduleB]
    it "removes everything if no `value` declarations has been found" $
      runDeclaration [D.Value]
        [moduleD, moduleG, moduleE, moduleH] `shouldBe` []
    it "extracts module by filtering `type` declarations" $
      runDeclaration [D.Type]
        [moduleA, moduleB, moduleC, moduleD, moduleE] `shouldBe` [moduleC]
    it "removes everything if a `type` declaration have not been found" $
      runDeclaration [D.Type]
        [moduleA, moduleG, moduleE, moduleH] `shouldBe` []
    it "extracts module by filtering `synonym` declarations" $
      runDeclaration [D.Synonym]
        [moduleA, moduleB, moduleD, moduleE] `shouldBe` [moduleE]
    it "removes everything if a `synonym` declaration have not been found" $
      runDeclaration [D.Synonym]
        [moduleA, moduleB, moduleC, moduleH] `shouldBe` []
    it "extracts module by filtering `constructor` declarations" $
      runDeclaration [D.DataConstructor]
        [moduleA, moduleB, moduleC, moduleF] `shouldBe` [moduleF]
    it "removes everything if a `constructor` declaration have not been found" $
      runDeclaration [D.DataConstructor]
        [moduleA, moduleB, moduleC, moduleH] `shouldBe` []
    it "extracts module by filtering `typeclass` declarations" $
      runDeclaration [D.TypeClass]
        [moduleA, moduleC, moduleG] `shouldBe` [moduleG]
    it "removes everything if a `typeclass` declaration have not been found" $
      runDeclaration [D.TypeClass]
        [moduleA, moduleB, moduleC, moduleH] `shouldBe` []
    it "extracts modules by filtering `valueoperator` declarations" $
      runDeclaration [D.ValueOperator]
        [moduleA, moduleC, moduleG, moduleH, moduleF] `shouldBe` [moduleH]
    it "removes everything if a `valueoperator` declaration have not been found" $
      runDeclaration [D.ValueOperator]
        [moduleA, moduleB, moduleC, moduleD] `shouldBe` []
    it "extracts modules by filtering `typeoperator` declarations" $
      runDeclaration [D.TypeOperator]
        [moduleA, moduleC, moduleG, moduleI, moduleF] `shouldBe` [moduleI]
    it "removes everything if a `typeoperator` declaration have not been found" $
      runDeclaration [D.TypeOperator]
        [moduleA, moduleD] `shouldBe` []
    it "extracts module by filtering `kind` declarations" $
      runDeclaration [D.Kind]
        [moduleA, moduleD, moduleG, moduleI, moduleF] `shouldBe` [moduleD]
    it "removes everything if a `kind` declaration have not been found" $
      runDeclaration [D.Kind]
        [moduleA, moduleC] `shouldBe` []
    it "extracts modules by filtering `value` and `synonym` declarations" $
      runDeclaration [D.Value, D.Synonym]
        [moduleA, moduleB, moduleD, moduleE] `shouldBe` [moduleA, moduleB, moduleE]
    it "extracts modules by filtering `value`, `kind`, and `valueoperator` declarations" $
      runDeclaration [D.Value, D.Kind, D.ValueOperator]
        [moduleA, moduleB, moduleD, moduleG, moduleE, moduleH] `shouldBe` [moduleA, moduleB, moduleD, moduleH]
