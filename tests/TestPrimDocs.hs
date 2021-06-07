module TestPrimDocs where

import Prelude

import Data.List (sort)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D

import Test.Hspec

spec :: Spec
spec = do
  it "there are no bottoms hiding in primModules" $ do
    _ <- evaluate (force D.primModules)
    return ()

  it "all Prim modules are fully documented" $ do
    let actualPrimNames =
          -- note that prim type classes are listed in P.primTypes
          filter (not . Text.any (== '$')) . map (P.runProperName . P.disqualify . fst) $ Map.toList
            ( P.primTypes <>
              P.primBooleanTypes <>
              P.primCoerceTypes <>
              P.primOrderingTypes <>
              P.primRowTypes <>
              P.primRowListTypes <>
              P.primTypeErrorTypes <>
              P.primSymbolTypes )
    let documentedPrimNames =
          map D.declTitle (concatMap D.modDeclarations D.primModules)

    sort documentedPrimNames `shouldBe` sort actualPrimNames
