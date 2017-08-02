{-# LANGUAGE OverloadedStrings #-}
module TestHierarchy where

import Language.PureScript.Hierarchy
import qualified Language.PureScript as P
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Language.PureScript.Hierarchy" $ do
    describe "typeClassGraph" $ do
      it "generates usable graphviz graphs" $ do
        let declarations =
              [ P.TypeClassDeclaration
                 (P.internalModuleSourceSpan "<A>", [])
                 (P.ProperName "A")
                 []
                 []
                 []
                 []
              , P.TypeClassDeclaration
                 (P.internalModuleSourceSpan "<B>", [])
                 (P.ProperName "B")
                 []
                 [P.Constraint (P.Qualified Nothing $ P.ProperName "A") [] Nothing]
                 []
                 []
              ]
        let mainModule = P.Module
              (P.internalModuleSourceSpan "<hierarchy>")
              []
              (P.ModuleName [P.ProperName "Main"])
              declarations
              Nothing

        let graph = typeClassGraph mainModule

        graph `shouldBe` Just (GraphName "Main", Graph "digraph Main {\n  A;\n  A -> B;\n}")
