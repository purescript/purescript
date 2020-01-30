module TestGraph where

import Prelude ()
import Prelude.Compat

import Test.Tasty
import Test.Tasty.Hspec
import System.FilePath
import System.IO.UTF8 (readUTF8FileT)
import Data.ByteString.Lazy as ByteString
import Data.Text.Encoding as Text

import qualified Language.PureScript as P

main :: IO TestTree
main = testSpec "graph" spec

spec :: Spec
spec = do
  let sourcesDir = "tests/purs/graph"
  it "should match the graph fixture" $ do
    let modulePaths = (\m -> sourcesDir </> "src" </> m) <$> ["Module.purs", "Module2.purs"]

    graphFixture <- readUTF8FileT (sourcesDir </> "graph.json")
    eitherGraph <- snd <$> P.graph modulePaths
    case eitherGraph of
      Left err -> error $ "Graph creation failed. Errors: " <> show err
      Right res ->
        let textRes = Text.decodeUtf8 $ ByteString.toStrict res
        in graphFixture `shouldBe` textRes
