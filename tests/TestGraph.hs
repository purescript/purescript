module TestGraph where

import Prelude

import Test.Hspec (Spec, it, shouldBe, shouldSatisfy)
import Data.Either (isLeft)

import Data.Aeson qualified as Json
import Language.PureScript qualified as P

spec :: Spec
spec = do
  let baseDir = "tests/purs/graph/"
  let sourcesDir = baseDir <> "src/"
  it "should match the graph fixture" $ do
    let modulePaths = (sourcesDir <>) <$> ["Module.purs", "Module2.purs", "Module3.purs"]
    let graphFixtureName = "graph.json"

    graphFixture <- Json.decodeFileStrict' (baseDir <> graphFixtureName)
    eitherGraph <- fst <$> P.graph modulePaths
    case eitherGraph of
      Left err -> error $ "Graph creation failed. Errors: " <> show err
      Right res -> graphFixture `shouldBe` Just res

  it "should fail when trying to include non-existing modules in the graph" $ do
    let modulePath = sourcesDir <> "ModuleFailing.purs"
    graph <- fst <$> P.graph [modulePath]
    graph `shouldSatisfy` isLeft
