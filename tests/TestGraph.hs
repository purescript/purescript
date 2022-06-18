module TestGraph where

import Prelude ()
import Prelude.Compat

import Test.Hspec
import System.IO.UTF8 (readUTF8FileT)
import Data.Either (isLeft)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as Json
import qualified Language.PureScript as P

spec :: Spec
spec = do
  let baseDir = "tests/purs/graph/"
  let sourcesDir = baseDir <> "src/"
  it "should match the graph fixture" $ do
    let modulePaths = (sourcesDir <>) <$> ["Module.purs", "Module2.purs", "Module3.purs"]
    let graphFixtureName = "graph.json"

    graphFixture <- readUTF8FileT (baseDir <> graphFixtureName)
    eitherGraph <- fst <$> P.graph modulePaths
    case eitherGraph of
      Left err -> error $ "Graph creation failed. Errors: " <> show err
      Right res -> do
        let graphFixture' = Json.decode $ ByteString.fromStrict $ Text.encodeUtf8 graphFixture
        graphFixture' `shouldBe` Just res

  it "should fail when trying to include non-existing modules in the graph" $ do
    let modulePath = sourcesDir <> "ModuleFailing.purs"
    graph <- fst <$> P.graph [modulePath]
    graph `shouldSatisfy` isLeft
