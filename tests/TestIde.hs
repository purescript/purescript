module TestIde where

import           Prelude

import           Control.Monad (unless)
import           Language.PureScript.Ide.Test
import qualified PscIdeSpec
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO TestTree
main =
  testSpec "ide" (beforeAll_ setup PscIdeSpec.spec)
  where
    setup = do
      deleteOutputFolder
      s <- compileTestProject
      unless s (fail "Failed to compile .purs sources")

