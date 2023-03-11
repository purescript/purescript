module TestPrimDocs where

import Prelude

import Data.List (sort)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Language.PureScript qualified as P
import Language.PureScript.Docs qualified as D

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
              P.primSymbolTypes <>
              P.primIntTypes )
    let documentedPrimNames =
          map D.declTitle (concatMap D.modDeclarations D.primModules)

    sort documentedPrimNames `shouldBe` sort actualPrimNames
