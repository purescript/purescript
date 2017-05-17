{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.CompletionSpec where

import Protolude

import Language.PureScript.Ide.Completion
import Language.PureScript.Ide.Test
import Language.PureScript.Ide.Types
import Test.Hspec

matches :: [Match IdeDeclarationAnn]
matches = map (\d -> Match (mn "Main", d)) [ ideKind "Kind", ideType "Type" Nothing ]

spec :: Spec
spec = describe "Applying completion options" $ do
  it "keeps all matches if maxResults is not specified" $ do
    applyCompletionOptions (defaultCompletionOptions { coMaxResults = Nothing }) matches
    `shouldBe`
      matches
  it "keeps only the specified amount of maxResults" $ do
    applyCompletionOptions (defaultCompletionOptions { coMaxResults = Just 1 }) matches
    `shouldBe`
      take 1 matches
