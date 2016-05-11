module TestPscIde where

import qualified PscIdeSpec
import Test.Hspec
import Language.PureScript.Ide.Integration

main :: IO ()
main = withServer (hspec PscIdeSpec.spec)
