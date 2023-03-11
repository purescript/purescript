module Language.PureScript.Ide.ReexportsSpec where

import Protolude

import Data.Map qualified as Map
import Language.PureScript.Ide.Reexports
    ( reexportHasFailures,
      resolveReexports',
      ReexportResult(reFailed, reResolved) )
import Language.PureScript.Ide.Types
    ( IdeDeclarationAnn, ModuleMap )
import Language.PureScript.Ide.Test
    ( ideType,
      annExp,
      ideKind,
      mn,
      ideValue,
      ideSynonym,
      ideTypeClass,
      ideDtor )
import Language.PureScript.AST.Declarations qualified as P
    ( DeclarationRef(TypeClassRef, ValueRef, TypeRef) )
import Language.PureScript.AST.SourcePos qualified as P
    ( internalModuleSourceSpan, SourceSpan )
import Language.PureScript.Environment qualified as P ( kindType )
import Language.PureScript.Names qualified as P
    ( Ident(Ident), ModuleName, ProperName(ProperName) )
import Test.Hspec ( shouldSatisfy, shouldBe, it, describe, Spec )

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
