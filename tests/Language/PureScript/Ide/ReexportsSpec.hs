{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.ReexportsSpec where

import           Protolude

import qualified Data.Map as Map
import           Language.PureScript.Ide.Reexports
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

m :: Text -> P.ModuleName
m = P.moduleNameFromString

d :: IdeDeclaration -> IdeDeclarationAnn
d = IdeDeclarationAnn emptyAnn

valueA, typeA, classA, dtorA1, dtorA2 :: IdeDeclarationAnn
valueA = d (IdeDeclValue (IdeValue (P.Ident "valueA") P.REmpty))
typeA = d (IdeDeclType (IdeType(P.ProperName "TypeA") P.kindType))
classA = d (IdeDeclTypeClass (IdeTypeClass (P.ProperName "ClassA") []))
dtorA1 = d (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName "DtorA1") (P.ProperName "TypeA") P.REmpty))
dtorA2 = d (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName "DtorA2") (P.ProperName "TypeA") P.REmpty))

env :: ModuleMap [IdeDeclarationAnn]
env = Map.fromList
  [ (m "A", [valueA, typeA, classA, dtorA1, dtorA2])
  ]

type Refs = [(P.ModuleName, P.DeclarationRef)]

succTestCases :: [(Text, [IdeDeclarationAnn], Refs, [IdeDeclarationAnn])]
succTestCases =
  [ ("resolves a value reexport", [], [(m "A", P.ValueRef (P.Ident "valueA"))], [valueA])
  , ("resolves a type reexport with explicit data constructors"
    , [], [(m "A", P.TypeRef (P.ProperName "TypeA") (Just [P.ProperName "DtorA1"]))], [typeA, dtorA1])
  , ("resolves a type reexport with implicit data constructors"
    , [], [(m "A", P.TypeRef (P.ProperName "TypeA") Nothing)], [typeA, dtorA1, dtorA2])
  , ("resolves a class reexport", [], [(m "A", P.TypeClassRef (P.ProperName "ClassA"))], [classA])
  ]

failTestCases :: [(Text, [IdeDeclarationAnn], Refs)]
failTestCases =
  [ ("fails to resolve a non existing value", [], [(m "A", P.ValueRef (P.Ident "valueB"))])
  , ("fails to resolve a non existing type reexport" , [], [(m "A", P.TypeRef (P.ProperName "TypeB") Nothing)])
  , ("fails to resolve a non existing class reexport", [], [(m "A", P.TypeClassRef (P.ProperName "ClassB"))])
  ]

spec :: Spec
spec = do
  describe "Successful Reexports" $
    for_ succTestCases $ \(desc, initial, refs, result) ->
      it (toS desc) $ do
        let reResult = resolveReexports' env initial refs
        reResolved reResult `shouldBe` result
        reResult `shouldSatisfy` not . reexportHasFailures
  describe "Failed Reexports" $
    for_ failTestCases $ \(desc, initial, refs) ->
      it (toS desc) $ do
        let reResult = resolveReexports'  env initial refs
        reFailed reResult `shouldBe` refs
        reResult `shouldSatisfy` reexportHasFailures
