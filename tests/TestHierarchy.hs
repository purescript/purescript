module TestHierarchy where

import System.Process (readCreateProcess, shell)

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

import TestUtils (pushd)

main :: IO ()
main = pushd "tests/support/hierarchy" (hspec spec)

spec :: Spec
spec = do
  describe "hierarchy" $ do
    it "generates usable graphviz graphs" $ do
      let expected = unlines
            [ "digraph Main {"
            , "  A;"
            , "  A -> B;"
            , "  C;"
            , "}"
            ]

      actual <- readCreateProcess (shell "purs hierarchy src/Main.purs") ""

      actual `shouldBe` expected
