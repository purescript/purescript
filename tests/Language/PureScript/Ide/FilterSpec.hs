{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.FilterSpec where

import           Protolude
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

value :: Text -> IdeDeclarationAnn
value s = IdeDeclarationAnn emptyAnn (IdeDeclValue (IdeValue (P.Ident (toS s)) P.REmpty))

valueOp :: Text -> Text -> IdeDeclarationAnn
valueOp s alias =
  IdeDeclarationAnn emptyAnn (IdeDeclValueOperator
                              (IdeValueOperator
                               (P.OpName (toS s))
                               (P.Qualified Nothing (Left (P.Ident (toS alias))))
                               1
                               P.Infix
                               Nothing
                              ))

moduleA, moduleB, moduleC :: Module
moduleA = (P.moduleNameFromString "Module.A", [value "function1"])
moduleB = (P.moduleNameFromString "Module.B", [value "data1"])
moduleC = (P.moduleNameFromString "Module.C", [valueOp "<<>>" "operator"])

modules :: [Module]
modules = [moduleA, moduleB, moduleC]

runEq :: Text -> [Module]
runEq s = applyFilters [equalityFilter s] modules

runPrefix :: Text -> [Module]
runPrefix s = applyFilters [prefixFilter s] modules

runModule :: [P.ModuleName] -> [Module]
runModule ms = applyFilters [moduleFilter ms] modules

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
    it "keeps operator alias matches" $
      runPrefix "oper" `shouldBe` [moduleC]
  describe "moduleFilter" $ do
    it "removes everything on empty input" $
      runModule [] `shouldBe` []
    it "only keeps the specified modules" $
      runModule [P.moduleNameFromString "Module.A"] `shouldBe` [moduleA]
    it "ignores modules that are not in scope" $
      runModule (P.moduleNameFromString <$> ["Module.A", "Unknown"]) `shouldBe` [moduleA]
