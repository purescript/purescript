module TestIde where

import           Prelude

import           Control.Monad (unless)
import           Language.PureScript.Ide.Test
import qualified PscIdeSpec
import           Test.Hspec

spec :: Spec
spec =
  beforeAll_ setup PscIdeSpec.spec
  where
    setup = do
      deleteOutputFolder
      s <- compileTestProject
      unless s (fail "Failed to compile .purs sources")

