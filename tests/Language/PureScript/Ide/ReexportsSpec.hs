{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.ReexportsSpec where

import           Protolude

import qualified Data.Map as Map
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Test
import qualified Language.PureScript as P
import           Test.Hspec

valueA, typeA, synonymA, classA, dtorA1, dtorA2, kindA :: IdeDeclarationAnn
valueA = ideValue "valueA" Nothing
typeA = ideType "TypeA" Nothing []
synonymA = ideSynonym "SynonymA" Nothing Nothing
classA = ideTypeClass "ClassA" P.kindType []
dtorA1 = ideDtor "DtorA1" "TypeA" Nothing
dtorA2 = ideDtor "DtorA2" "TypeA" Nothing
kindA = ideKind "KindA"

env :: ModuleMap [IdeDeclarationAnn]
env = Map.fromList
  [ (mn "A", [valueA, typeA, synonymA, classA, dtorA1, dtorA2, kindA])
  ]

type Refs = [(P.ModuleName, P.DeclarationRef)]

testSpan :: P.SourceSpan
testSpan = P.internalModuleSourceSpan "<test>"

succTestCases :: [(Text, Refs, [IdeDeclarationAnn])]
succTestCases =
  [ ("resolves a value reexport", [(mn "A", P.ValueRef testSpan (P.Ident "valueA"))], [valueA `annExp` "A"])
  , ("resolves a type reexport with explicit data constructors"
    , [(mn "A", P.TypeRef testSpan (P.ProperName "TypeA") (Just [P.ProperName "DtorA1"]))], [typeA `annExp` "A", dtorA1 `annExp` "A"])
  , ("resolves a type reexport with implicit data constructors"
    , [(mn "A", P.TypeRef testSpan (P.ProperName "TypeA") Nothing)], map (`annExp` "A") [typeA, dtorA1, dtorA2])
  , ("resolves a synonym reexport"
    , [(mn "A", P.TypeRef testSpan (P.ProperName "SynonymA") Nothing)], [synonymA `annExp` "A"])
  , ("resolves a class reexport", [(mn "A", P.TypeClassRef testSpan (P.ProperName "ClassA"))], [classA `annExp` "A"])
  , ("resolves a kind reexport", [(mn "A", P.KindRef testSpan (P.ProperName "KindA"))], [kindA `annExp` "A"])
  ]

failTestCases :: [(Text, Refs)]
failTestCases =
  [ ("fails to resolve a non existing value", [(mn "A", P.ValueRef testSpan (P.Ident "valueB"))])
  , ("fails to resolve a non existing type reexport" , [(mn "A", P.TypeRef testSpan (P.ProperName "TypeB") Nothing)])
  , ("fails to resolve a non existing class reexport", [(mn "A", P.TypeClassRef testSpan (P.ProperName "ClassB"))])
  ]

spec :: Spec
spec = do
  describe "Successful Reexports" $
    for_ succTestCases $ \(desc, refs, result) ->
      it (toS desc) $ do
        let reResult = resolveReexports' env refs
        reResolved reResult `shouldBe` result
        reResult `shouldSatisfy` not . reexportHasFailures
  describe "Failed Reexports" $
    for_ failTestCases $ \(desc, refs) ->
      it (toS desc) $ do
        let reResult = resolveReexports'  env refs
        reFailed reResult `shouldBe` refs
        reResult `shouldSatisfy` reexportHasFailures
