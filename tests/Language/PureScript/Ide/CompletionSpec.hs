{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.CompletionSpec where

import Protolude

import Language.PureScript as P
import Language.PureScript.Ide.Completion
import Language.PureScript.Ide.Test
import Language.PureScript.Ide.Types
import Test.Hspec

reexportMatches :: [Match IdeDeclarationAnn]
reexportMatches =
  map (\d -> Match (mn "A", d)) moduleA
  ++ map (\d -> Match (mn "B", d)) moduleB
  where
    moduleA = [ideKind "Kind"]
    moduleB = [ideKind "Kind" `annExp` "A"]

matches :: [(Match IdeDeclarationAnn, [P.ModuleName])]
matches = map (\d -> (Match (mn "Main", d), [mn "Main"])) [ ideKind "Kind", ideType "Type" Nothing [] ]

spec :: Spec
spec = describe "Applying completion options" $ do
  it "keeps all matches if maxResults is not specified" $ do
    applyCompletionOptions (defaultCompletionOptions { coMaxResults = Nothing })
      (map fst matches) `shouldMatchList` matches
  it "keeps only the specified amount of maxResults" $ do
    applyCompletionOptions (defaultCompletionOptions { coMaxResults = Just 1 })
      (map fst matches) `shouldMatchList` take 1 matches
  it "groups reexports for a single identifier" $ do
    applyCompletionOptions (defaultCompletionOptions { coGroupReexports = True })
      reexportMatches `shouldBe` [(Match (mn "A", ideKind "Kind"), [mn "A", mn "B"])]
