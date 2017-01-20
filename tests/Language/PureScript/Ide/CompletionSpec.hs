{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.CompletionSpec where

import Protolude
import qualified Data.Text as Text
import Language.PureScript.Ide.Completion
import Test.Hspec

shouldInsertHole :: Text -> Text -> Expectation
shouldInsertHole input output =
  let
    Just col = Text.findIndex (== '|') input
  in
    insertHole (1, col) (Text.filter (/= '|') input) `shouldBe` output <> "\n"

spec :: Spec
spec = describe "Inserting typed holes" $ do
  it "inserts a typed hole for a record accessor" $ do
    "record.|" `shouldInsertHole` "(?pscIdeHole record)"
  it "inserts a typed hole for a record accessor and returns its prefix" $ do
    pending
    -- "record.pre|" `shouldInsertHole` "(?pscIdeHole record)"
  it "inserts a typed hole for a wildcard record accessor" $ do
    "_.|" `shouldInsertHole` "?pscIdeHole"

