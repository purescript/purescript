{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.ReexportsSpec where

import qualified Prelude
import           Protolude

import qualified Data.Map as Map
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

m :: Prelude.String -> P.ModuleName
m = P.moduleNameFromString

d :: IdeDeclaration -> IdeDeclarationAnn
d = IdeDeclarationAnn emptyAnn

valueA, typeA, classA, dtorA1, dtorA2 :: IdeDeclarationAnn
valueA = d (IdeValue (P.Ident "valueA") P.REmpty)
typeA = d (IdeType (P.ProperName "TypeA") P.Star)
classA = d (IdeTypeClass (P.ProperName "ClassA"))
dtorA1 = d (IdeDataConstructor (P.ProperName "DtorA1") (P.ProperName "TypeA") P.REmpty)
dtorA2 = d (IdeDataConstructor (P.ProperName "DtorA2") (P.ProperName "TypeA") P.REmpty)

env :: Map P.ModuleName [IdeDeclarationAnn]
env = Map.fromList
  [ (m "A", [valueA, typeA, classA, dtorA1, dtorA2])
  ]

type Refs = [(P.ModuleName, P.DeclarationRef)]

succTestCases :: [(Text, Module, Refs, Module)]
succTestCases =
  [ ("resolves a value reexport", (m "C", []), [(m "A", P.ValueRef (P.Ident "valueA"))], (m "C", [valueA]))
  , ("resolves a type reexport with explicit data constructors"
    , (m "C", []), [(m "A", P.TypeRef (P.ProperName "TypeA") (Just [P.ProperName "DtorA1"]))], (m "C", [typeA, dtorA1]))
  , ("resolves a type reexport with implicit data constructors"
    , (m "C", []), [(m "A", P.TypeRef (P.ProperName "TypeA") Nothing)], (m "C", [typeA, dtorA1, dtorA2]))
  , ("resolves a class reexport", (m "C", []), [(m "A", P.TypeClassRef (P.ProperName "ClassA"))], (m "C", [classA]))
  ]

failTestCases :: [(Text, Module, Refs)]
failTestCases =
  [ ("fails to resolve a non existing value", (m "C", []), [(m "A", P.ValueRef (P.Ident "valueB"))])
  , ("fails to resolve a non existing type reexport" , (m "C", []), [(m "A", P.TypeRef (P.ProperName "TypeB") Nothing)])
  , ("fails to resolve a non existing class reexport", (m "C", []), [(m "A", P.TypeClassRef (P.ProperName "ClassB"))])
  ]

spec :: Spec
spec = do
  describe "Successful Reexports" $
    for_ succTestCases $ \(desc, initial, refs, result) ->
      it (toS desc) $ do
        let reResult = resolveReexports env (initial, refs)
        reResolved reResult `shouldBe` result
        reResult `shouldSatisfy` not . reexportHasFailures
  describe "Failed Reexports" $
    for_ failTestCases $ \(desc, initial, refs) ->
      it (toS desc) $ do
        let reResult = resolveReexports env (initial, refs)
        reFailed reResult `shouldBe` refs
        reResult `shouldSatisfy` reexportHasFailures
