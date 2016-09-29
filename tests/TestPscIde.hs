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

  quitServer -- shuts down any left over server (primarily happens during development)
  withServer (hspec PscIdeSpec.spec)
