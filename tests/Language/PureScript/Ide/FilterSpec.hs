module Language.PureScript.Ide.FilterSpec where

import Protolude
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Ide.Filter
import Language.PureScript.Ide.Filter.Declaration as D
import Language.PureScript.Ide.Types
import Language.PureScript.Ide.Imports
import Language.PureScript.Ide.Test as T
import Language.PureScript qualified as P
import Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

moduleA, moduleB, moduleC, moduleD, moduleE, moduleF, moduleG, moduleH, moduleI, moduleDCtors :: Module
moduleA = (P.moduleNameFromString "Module.A", [T.ideValue "function1" Nothing])
moduleB = (P.moduleNameFromString "Module.B", [T.ideValue "data1" Nothing])
moduleC = (P.moduleNameFromString "Module.C", [T.ideType "List" Nothing []])
moduleD = (P.moduleNameFromString "Module.D", [T.ideType "kind1" Nothing []])
moduleE = (P.moduleNameFromString "Module.E", [T.ideSynonym "SFType" Nothing Nothing `annLoc` synonymSS])
moduleF = (P.moduleNameFromString "Module.F", [T.ideDtor "DtorA" "TypeA" Nothing])
moduleG = (P.moduleNameFromString "Module.G", [T.ideTypeClass "MyClass" P.kindType []])
moduleH = (P.moduleNameFromString "Module.H", [T.ideValueOp "<$>" (P.Qualified P.ByNullSourcePos (Left "")) 0 Nothing Nothing])
moduleI = (P.moduleNameFromString "Module.I", [T.ideTypeOp "~>" (P.Qualified P.ByNullSourcePos "") 0 Nothing Nothing])
moduleDCtors = (P.moduleNameFromString "Module.WithDC", [T.ideType "Foo" Nothing [(P.ProperName "A", P.tyString), (P.ProperName "B", P.tyString)] ])

modules :: ModuleMap [IdeDeclarationAnn]
modules = Map.fromList [moduleA, moduleB]

allModules :: ModuleMap [IdeDeclarationAnn]
allModules = Map.fromList [moduleA, moduleB,moduleC,moduleD,moduleE,moduleF,moduleG,moduleH,moduleI,moduleDCtors]

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

runDependency :: [Text] -> [Module]
runDependency = runDependency' "Whatever"

runDependency' :: Text -> [Text] -> [Module]
runDependency' currentModuleName imports = Map.toList $ applyFilters [dependencyFilter Nothing (P.ModuleName currentModuleName) (testParseImports currentModuleName imports)] allModules

runDependencyQualified :: Text -> [Text] -> [Module]
runDependencyQualified qualifier imports = Map.toList $ applyFilters [dependencyFilter (Just $ P.ModuleName qualifier) (P.ModuleName "Whatever") (testParseImports "Whatever" imports)] allModules

testParseImports :: Text -> [Text] -> [Import]
testParseImports currentModuleName imports = either (const []) (\(_, _, x, _) -> x) $ sliceImportSection moduleLines
  where
  moduleLines = "module " <> currentModuleName <> " where" : (imports <> [ "", "blah = 42" ])

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
    it "extracts modules by filtering `value` and `type` namespaces" $
      runNamespace (Set.fromList [ IdeNSValue, IdeNSType])
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
        [moduleA, moduleB, moduleC, moduleD, moduleE] `shouldBe` [moduleC, moduleD]
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
    it "extracts modules by filtering `value` and `synonym` declarations" $
      runDeclaration [D.Value, D.Synonym]
        [moduleA, moduleB, moduleD, moduleE] `shouldBe` [moduleA, moduleB, moduleE]
    it "extracts modules by filtering `value`, and `valueoperator` declarations" $
      runDeclaration [D.Value, D.ValueOperator]
        [moduleA, moduleB, moduleD, moduleG, moduleE, moduleH] `shouldBe` [moduleA, moduleB, moduleH]
  describe "dependencyFilter" $ do
    describe "import types" $ do
      it "filters by implicit imports" $ do
        runDependency ["import Module.A", "import Module.C"] `shouldBe` [moduleA, moduleC]
      it "filters by matching explicit value import" $ do
        runDependency ["import Module.A (function1)"] `shouldBe` [moduleA]
      it "filters by matching explicit value import from correct module" $ do
        runDependency ["import Module.B (function1)"] `shouldBe` []
      it "filters not matching explicit value import" $ do
        runDependency ["import Module.A (function2)"] `shouldBe` []
      it "filters out names in hiding import" $ do
        runDependency ["import Module.A hiding (function1)"] `shouldBe` []
      it "doesn't filter out not matching names in hiding import" $ do
        runDependency ["import Module.A hiding (nonsense)"] `shouldBe` [moduleA]
      it "filters by containing module" $ do
        runDependency' "Module.A" ["import Module.Blah"] `shouldBe` [moduleA]
    describe "declaration types" $ do
      it "matches type" $ do
        runDependency ["import Module.C (List)"] `shouldBe` [moduleC]
      it "includes data constructor with (..)" $ do
        runDependency ["import Module.F (TypeA(..))"] `shouldBe` [moduleF]
      it "includes data constructor explicitly listed" $ do
        runDependency ["import Module.F (TypeA(DtorA))"] `shouldBe` [moduleF]
      it "does not include data constructor not explicitly listed" $ do
        runDependency ["import Module.F (TypeA(BogusOtherConstructor))"] `shouldBe` []
      it "does not include data constructor when only the type is imported" $ do
        runDependency ["import Module.F (TypeA)"] `shouldBe` []
      it "includes synonym" $ do
        runDependency ["import Module.E (SFType)"] `shouldBe` [moduleE]
      it "includes typeclass" $ do
        runDependency ["import Module.G (class MyClass)"] `shouldBe` [moduleG]
      it "includes value op" $ do
        runDependency ["import Module.H ((<$>))"] `shouldBe` [moduleH]
      it "includes type op" $ do
        runDependency ["import Module.I (type (~>))"] `shouldBe` [moduleI]
    describe "qualifiers" $ do
      it "includes single qualified import and not unqualified things" $ do
        runDependencyQualified "AA" [ "import Module.A as AA", "import Module.C"] `shouldBe` [moduleA]
      it "includes multiple qualified imports" $ do
        runDependencyQualified "Combined.Thing" [ "import Module.A as Combined.Thing", "import Module.C as Combined.Thing", "import Module.F"] `shouldBe` [moduleA, moduleC]
      it "doesn't include qualified import when qualifier not specified" $ do
        runDependency [ "import Module.AA as A"] `shouldBe` []
