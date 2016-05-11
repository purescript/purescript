module TestPscIde where

import           Control.Monad                       (unless)
import           Language.PureScript.Ide.Integration
import qualified PscIdeSpec
import           Test.Hspec

main :: IO ()
main = do
  deleteOutputFolder
  s <- compileTestProject
  unless s $ fail "Failed to compile .purs sources"

  withServer (hspec PscIdeSpec.spec)
