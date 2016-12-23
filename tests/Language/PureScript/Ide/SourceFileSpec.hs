{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.SourceFileSpec where

import           Protolude

import qualified Language.PureScript as P
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.Types
import           Test.Hspec

span0, span1, span2 :: P.SourceSpan
span0 = P.SourceSpan "ModuleLevel" (P.SourcePos 0 0) (P.SourcePos 1 1)
span1 = P.SourceSpan "" (P.SourcePos 1 1) (P.SourcePos 2 2)
span2 = P.SourceSpan "" (P.SourcePos 2 2) (P.SourcePos 3 3)

typeAnnotation1, value1, synonym1, class1, class2, data1, data2, foreign1, foreign2, foreign3, member1 :: P.Declaration
typeAnnotation1 = P.TypeDeclaration (P.Ident "value1") P.REmpty
value1 = P.ValueDeclaration (P.Ident "value1") P.Public [] (Left [])
synonym1 = P.TypeSynonymDeclaration (P.ProperName "Synonym1") [] P.REmpty
class1 = P.TypeClassDeclaration (P.ProperName "Class1") [] [] [] []
class2 = P.TypeClassDeclaration (P.ProperName "Class2") [] [] []
  [P.PositionedDeclaration span2 [] member1]
data1 = P.DataDeclaration P.Newtype (P.ProperName "Data1") [] []
data2 = P.DataDeclaration P.Data (P.ProperName "Data2") [] [(P.ProperName "Cons1", [])]
foreign1 = P.ExternDeclaration (P.Ident "foreign1") P.REmpty
foreign2 = P.ExternDataDeclaration (P.ProperName "Foreign2") P.kindType
foreign3 = P.ExternKindDeclaration (P.ProperName "Foreign3")
member1 = P.TypeDeclaration (P.Ident "member1") P.REmpty

spec :: Spec
spec = do
  describe "Extracting Spans" $ do
    it "extracts a span for a value declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] value1) `shouldBe` [(IdeNSValue "value1", span1)]
    it "extracts a span for a type synonym declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] synonym1) `shouldBe` [(IdeNSType "Synonym1", span1)]
    it "extracts a span for a typeclass declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] class1) `shouldBe` [(IdeNSType "Class1", span1)]
    it "extracts spans for a typeclass declaration and its members" $
      extractSpans span0 (P.PositionedDeclaration span1 [] class2) `shouldBe` [(IdeNSType "Class2", span1), (IdeNSValue "member1", span2)]
    it "extracts a span for a data declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] data1) `shouldBe` [(IdeNSType "Data1", span1)]
    it "extracts spans for a data declaration and its constructors" $
      extractSpans span0 (P.PositionedDeclaration span1 [] data2) `shouldBe` [(IdeNSType "Data2", span1), (IdeNSValue "Cons1", span1)]
    it "extracts a span for a foreign declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] foreign1) `shouldBe` [(IdeNSValue "foreign1", span1)]
    it "extracts a span for a data foreign declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] foreign2) `shouldBe` [(IdeNSType "Foreign2", span1)]
    it "extracts a span for a foreign kind declaration" $
      extractSpans span0 (P.PositionedDeclaration span1 [] foreign3) `shouldBe` [(IdeNSKind "Foreign3", span1)]
  describe "Type annotations" $ do
    it "extracts a type annotation" $
      extractTypeAnnotations [typeAnnotation1] `shouldBe` [(P.Ident "value1", P.REmpty)]
