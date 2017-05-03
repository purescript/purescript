module TestPscIde where

import           Control.Monad                       (unless)
import qualified PscIdeSpec
import           Language.PureScript.Ide.Test
import           Test.Hspec

main :: IO ()
main = do
  deleteOutputFolder
  s <- compileTestProject
  unless s (fail "Failed to compile .purs sources")
  hspec PscIdeSpec.spec
