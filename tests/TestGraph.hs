module TestGraph where

import Prelude ()
import Prelude.Compat

import Test.Tasty
import Test.Tasty.Hspec
import System.IO.UTF8 (readUTF8FileT)
import Data.Either (isLeft)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as Json
import qualified Language.PureScript as P


main :: IO TestTree
main = testSpec "graph" spec

spec :: Spec
spec = do
  let baseDir = "tests/purs/graph/"
  let sourcesDir = baseDir <> "src/"
  it "should match the graph fixture" $ do
    let modulePaths = (sourcesDir <>) <$> ["Module.purs", "Module2.purs"]
    let graphFixtureName = "graph.json"

    graphFixture <- readUTF8FileT (baseDir <> graphFixtureName)
    eitherGraph <- fst <$> P.graph modulePaths
    case eitherGraph of
      Left err -> error $ "Graph creation failed. Errors: " <> show err
      Right res ->
        let textRes = Text.decodeUtf8 $ ByteString.toStrict $ Json.encode res
        in graphFixture `shouldBe` textRes

  it "should fail when trying to include non-existing modules in the graph" $ do
    let modulePath = sourcesDir <> "ModuleFailing.purs"
    graph <- fst <$> P.graph [modulePath]
    graph `shouldSatisfy` isLeft
