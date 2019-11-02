module TestPrimDocs where

import Prelude

import Data.List (sort)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D

import Test.Tasty
import Test.Tasty.Hspec (Spec, testSpec, it)
import Test.Hspec (shouldBe)

main :: IO TestTree
main = testSpec "prim docs" spec

spec :: Spec
spec = do
  it "there are no bottoms hiding in primModules" $ do
    _ <- evaluate (force D.primModules)
    return ()

  it "all Prim modules are fully documented" $ do
    let actualPrimNames =
          -- note that prim type classes are listed in P.primTypes
          (map (P.runProperName . P.disqualify . fst) $ Map.toList
            ( P.primTypes <>
              P.primBooleanTypes <>
              P.primOrderingTypes <>
              P.primRowTypes <>
              P.primRowListTypes <>
              P.primTypeErrorTypes <>
              P.primSymbolTypes )) ++
          (map (P.runProperName . P.disqualify) $ Set.toList P.allPrimKinds)
    let documentedPrimNames =
          map D.declTitle (concatMap D.modDeclarations D.primModules)

    sort documentedPrimNames `shouldBe` sort actualPrimNames
