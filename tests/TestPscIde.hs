module TestPscIde where

import qualified PscIdeSpec
import Test.Hspec

main :: IO ()
main = hspec PscIdeSpec.spec
