{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.FilterSpec where

import           Data.Text                      (Text)
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

value :: Text -> ExternDecl
value s = ValueDeclaration s $ P.TypeWildcard $ P.SourceSpan "" (P.SourcePos 0 0) (P.SourcePos 0 0)

modules :: [Module]
modules =
  [
    ("Module.A", [value "function1"]),
    ("Module.B", [value "data1"]),
    ("Module.C", [ModuleDecl "Module.C" []]),
    ("Module.D", [Dependency "Module.C" [] Nothing, value "asd"])
  ]

runEq :: Text -> [Module]
runEq s = runFilter (equalityFilter s) modules
runPrefix :: Text -> [Module]
runPrefix s = runFilter (prefixFilter s) modules
runModule :: [ModuleIdent] -> [Module]
runModule ms = runFilter (moduleFilter ms) modules
runDependency :: [ModuleIdent] -> [Module]
runDependency ms = runFilter (dependencyFilter ms) modules

spec :: Spec
spec = do
  describe "equality Filter" $ do
    it "removes empty modules" $
      runEq "test" `shouldBe` []
    it "keeps function declarations that are equal" $
      runEq "function1" `shouldBe` [head modules]
    -- TODO: It would be more sensible to match Constructors
    it "keeps data declarations that are equal" $
      runEq "data1" `shouldBe` [modules !! 1]
  describe "prefixFilter" $ do
    it "keeps everything on empty string" $
      runPrefix "" `shouldBe` modules
    it "keeps functionname prefix matches" $
      runPrefix "fun" `shouldBe` [head modules]
    it "keeps data decls prefix matches" $
      runPrefix "dat" `shouldBe` [modules !! 1]
    it "keeps module decl prefix matches" $
      runPrefix "Mod" `shouldBe` [modules !! 2]
  describe "moduleFilter" $ do
    it "removes everything on empty input" $
      runModule [] `shouldBe` []
    it "only keeps the specified modules" $
      runModule ["Module.A", "Module.C"] `shouldBe` [head modules, modules !! 2]
    it "ignores modules that are not in scope" $
      runModule ["Module.A", "Module.C", "Unknown"] `shouldBe` [head modules, modules !! 2]
  describe "dependencyFilter" $ do
    it "removes everything on empty input" $
      runDependency [] `shouldBe` []
    it "only keeps the specified modules if they have no imports" $
      runDependency ["Module.A", "Module.B"] `shouldBe` [head modules, modules !! 1]
    it "keeps the specified modules and their imports" $
      runDependency ["Module.A", "Module.D"] `shouldBe` [head modules, modules !! 2, modules !! 3]
