{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.ReexportsSpec where

import           Control.Exception                 (evaluate)
import           Data.List                         (sort)
import qualified Data.Map                          as Map
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

wildcard :: P.Type
wildcard = P.TypeWildcard $ P.SourceSpan "" (P.SourcePos 0 0) (P.SourcePos 0 0)

decl1 :: ExternDecl
decl1 = ValueDeclaration "filter" wildcard
decl2 :: ExternDecl
decl2 = ValueDeclaration "map" wildcard
decl3 :: ExternDecl
decl3 = ValueDeclaration "catMaybe" wildcard
dep1 :: ExternDecl
dep1 = Dependency "Test.Foo" [] (Just "T")
dep2 :: ExternDecl
dep2 = Dependency "Test.Bar" [] (Just "T")

circularModule :: Module
circularModule = ("Circular", [Export "Circular"])

module1 :: Module
module1 = ("Module1", [Export "Module2", Export "Module3", decl1])

module2 :: Module
module2 = ("Module2", [decl2])

module3 :: Module
module3 = ("Module3", [decl3])

module4 :: Module
module4 = ("Module4", [Export "T", decl1, dep1, dep2])

result :: Module
result = ("Module1", [decl1, decl2, Export "Module3"])

db :: Map.Map ModuleIdent [ExternDecl]
db = Map.fromList [module1, module2, module3]

shouldBeEqualSorted :: Module -> Module -> Expectation
shouldBeEqualSorted (n1, d1) (n2, d2) = (n1, sort d1) `shouldBe` (n2, sort d2)

spec :: Spec
spec =
  describe "Reexports" $ do
    it "finds all reexports" $
      getReexports module1 `shouldBe` [Export "Module2", Export "Module3"]

    it "replaces a reexport with another module" $
      replaceReexport (Export "Module2") module1 module2 `shouldBeEqualSorted` result

    it "adds another module even if there is no export statement" $
      replaceReexport (Export "Module2") ("Module1", [decl1, Export "Module3"]) module2
      `shouldBeEqualSorted` result

    it "only adds a declaration once" $
      let replaced = replaceReexport (Export "Module2") module1 module2
      in replaceReexport (Export "Module2") replaced module2  `shouldBeEqualSorted` result

    it "should error when given a non-Export to replace" $
      evaluate (replaceReexport decl1 module1 module2)
      `shouldThrow` errorCall "Should only get Exports here."
    it "replaces all Exports with their corresponding declarations" $
      replaceReexports module1 db `shouldBe` ("Module1", [decl1, decl2, decl3])

    it "does not list itself as a reexport" $
      getReexports circularModule `shouldBe` []

    it "does not include circular references when replacing reexports" $
      replaceReexports circularModule (uncurry Map.singleton circularModule )
      `shouldBe` ("Circular", [])

    it "replaces exported aliases with imported module" $
      getReexports module4 `shouldBe` [Export "Test.Foo", Export "Test.Bar"]
