{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.SourceFileSpec where

import           Protolude

import qualified Language.PureScript as P
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.SourceFile
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Test
import           Test.Hspec

span0, span1, span2 :: P.SourceSpan
span0 = P.SourceSpan "ModuleLevel" (P.SourcePos 0 0) (P.SourcePos 1 1)
span1 = P.SourceSpan "" (P.SourcePos 1 1) (P.SourcePos 2 2)
span2 = P.SourceSpan "" (P.SourcePos 2 2) (P.SourcePos 3 3)

ann0, ann1, ann2 :: P.SourceAnn
ann0 = (span0, [])
ann1 = (span1, [])
ann2 = (span2, [])

typeAnnotation1, value1, synonym1, class1, class2, data1, data2, valueFixity, typeFixity, foreign1, foreign2, foreign3, member1 :: P.Declaration
typeAnnotation1 = P.TypeDeclaration (P.TypeDeclarationData ann1 (P.Ident "value1") P.REmpty)
value1 = P.ValueDecl ann1 (P.Ident "value1") P.Public [] []
synonym1 = P.TypeSynonymDeclaration ann1 (P.ProperName "Synonym1") [] P.REmpty
class1 = P.TypeClassDeclaration ann1 (P.ProperName "Class1") [] [] [] []
class2 = P.TypeClassDeclaration ann1 (P.ProperName "Class2") [] [] [] [member1]
data1 = P.DataDeclaration ann1 P.Newtype (P.ProperName "Data1") [] []
data2 = P.DataDeclaration ann1 P.Data (P.ProperName "Data2") [] [(P.ProperName "Cons1", [])]
valueFixity =
  P.ValueFixityDeclaration
    ann1
    (P.Fixity P.Infix 0)
    (P.Qualified Nothing (Left (P.Ident "")))
    (P.OpName "<$>")
typeFixity =
  P.TypeFixityDeclaration
    ann1
    (P.Fixity P.Infix 0)
    (P.Qualified Nothing (P.ProperName ""))
    (P.OpName "~>")
foreign1 = P.ExternDeclaration ann1 (P.Ident "foreign1") P.REmpty
foreign2 = P.ExternDataDeclaration ann1 (P.ProperName "Foreign2") P.kindType
foreign3 = P.ExternKindDeclaration ann1 (P.ProperName "Foreign3")
member1 = P.TypeDeclaration (P.TypeDeclarationData ann2 (P.Ident "member1") P.REmpty)

spec :: Spec
spec = do
  describe "Extracting Spans" $ do
    it "extracts a span for a value declaration" $
      extractSpans value1 `shouldBe` [(IdeNamespaced IdeNSValue "value1", span1)]
    it "extracts a span for a type synonym declaration" $
      extractSpans synonym1 `shouldBe` [(IdeNamespaced IdeNSType "Synonym1", span1)]
    it "extracts a span for a typeclass declaration" $
      extractSpans class1 `shouldBe` [(IdeNamespaced IdeNSType "Class1", span1)]
    it "extracts spans for a typeclass declaration and its members" $
      extractSpans class2 `shouldBe` [(IdeNamespaced IdeNSType "Class2", span1), (IdeNamespaced IdeNSValue "member1", span2)]
    it "extracts a span for a data declaration" $
      extractSpans data1 `shouldBe` [(IdeNamespaced IdeNSType "Data1", span1)]
    it "extracts spans for a data declaration and its constructors" $
      extractSpans data2 `shouldBe` [(IdeNamespaced IdeNSType "Data2", span1), (IdeNamespaced IdeNSValue "Cons1", span1)]
    it "extracts a span for a value operator fixity declaration" $
      extractSpans valueFixity `shouldBe` [(IdeNamespaced IdeNSValue "<$>", span1)]
    it "extracts a span for a type operator fixity declaration" $
      extractSpans typeFixity `shouldBe` [(IdeNamespaced IdeNSType "~>", span1)]
    it "extracts a span for a foreign declaration" $
      extractSpans foreign1 `shouldBe` [(IdeNamespaced IdeNSValue "foreign1", span1)]
    it "extracts a span for a data foreign declaration" $
      extractSpans foreign2 `shouldBe` [(IdeNamespaced IdeNSType "Foreign2", span1)]
    it "extracts a span for a foreign kind declaration" $
      extractSpans foreign3 `shouldBe` [(IdeNamespaced IdeNSKind "Foreign3", span1)]
  describe "Type annotations" $ do
    it "extracts a type annotation" $
      extractTypeAnnotations [typeAnnotation1] `shouldBe` [(P.Ident "value1", P.REmpty)]
  describe "Finding Source Spans for identifiers" $ do
    it "finds a value declaration" $ do
      Just r <- getLocation "sfValue"
      r `shouldBe` valueSS
    it "finds a synonym declaration" $ do
      Just r <- getLocation "SFType"
      r `shouldBe` synonymSS
    it "finds a data declaration and its constructors" $ do
      rs <- traverse getLocation ["SFData", "SFOne", "SFTwo", "SFThree"]
      traverse_ (`shouldBe` (Just typeSS)) rs
    it "finds a class declaration" $ do
      Just r <- getLocation "SFClass"
      r `shouldBe` classSS
    it "finds a value operator declaration" $ do
      Just r <- getLocation "<$>"
      r `shouldBe` valueOpSS
    it "finds a type operator declaration" $ do
      Just r <- getLocation "~>"
      r `shouldBe` typeOpSS

getLocation :: Text -> IO (Maybe P.SourceSpan)
getLocation s = do
  ([Right (CompletionResult [c])], _) <-
    runIde' defConfig ideState [Type s [] Nothing]
  pure (complLocation c)
  where
    ideState = emptyIdeState `volatileState`
      [ ("Test",
         [ ideValue "sfValue" Nothing `annLoc` valueSS
         , ideSynonym "SFType" Nothing Nothing `annLoc` synonymSS
         , ideType "SFData" Nothing [] `annLoc` typeSS
         , ideDtor "SFOne" "SFData" Nothing `annLoc` typeSS
         , ideDtor "SFTwo" "SFData" Nothing `annLoc` typeSS
         , ideDtor "SFThree" "SFData" Nothing `annLoc` typeSS
         , ideTypeClass "SFClass" P.kindType [] `annLoc` classSS
         , ideValueOp "<$>" (P.Qualified Nothing (Left "")) 0 Nothing Nothing
           `annLoc` valueOpSS
         , ideTypeOp "~>" (P.Qualified Nothing "") 0 Nothing Nothing
           `annLoc` typeOpSS
         ])
      ]
