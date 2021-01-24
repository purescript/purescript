{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.CompletionSpec where

import Protolude

import qualified Data.Set as Set
import qualified Language.PureScript as P
import Language.PureScript.Ide.Test as Test
import Language.PureScript.Ide.Command as Command
import Language.PureScript.Ide.Completion
import qualified Language.PureScript.Ide.Filter.Declaration as DeclarationType
import Language.PureScript.Ide.Types
import Test.Hspec
import System.FilePath

reexportMatches :: [Match IdeDeclarationAnn]
reexportMatches =
  map (\d -> Match (mn "A", d)) moduleA
  ++ map (\d -> Match (mn "B", d)) moduleB
  where
    moduleA = [ideKind "Kind"]
    moduleB = [ideKind "Kind" `annExp` "A"]

matches :: [(Match IdeDeclarationAnn, [P.ModuleName])]
matches = map (\d -> (Match (mn "Main", d), [mn "Main"])) [ ideKind "Kind", ideType "Type" Nothing [] ]

typ :: Text -> Command
typ txt = Type txt [] Nothing

load :: [Text] -> Command
load = LoadSync . map Test.mn

rebuildSync :: FilePath -> Command
rebuildSync fp = RebuildSync ("src" </> fp) Nothing (Set.singleton P.JS)

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

  it "gets simple docs on definition itself" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpecDocs"]
                  , typ "something"
                  ]
    result `shouldSatisfy` \res -> complDocumentation res == Just "Doc x\n"

  it "gets multiline docs" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpecDocs"]
                  , typ "multiline"
                  ]
    result `shouldSatisfy` \res -> complDocumentation res == Just "This is\na multi-line\ncomment\n"

  it "gets simple docs on type annotation" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpecDocs"]
                  , typ "withType"
                  ]
    result `shouldSatisfy` \res -> complDocumentation res == Just "Doc *123*\n"

  it "gets docs on module declaration" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpecDocs"]
                  , typ "CompletionSpecDocs"
                  ]
    result `shouldSatisfy` \res -> complDocumentation res == Just "Module Documentation\n"

  it "gets docs on type class declaration" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpecDocs"]
                  , typ "DocClass"
                  ]
    result `shouldSatisfy` \res -> complDocumentation res == Just "Doc for class\n"

  it "gets docs on type class members" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpecDocs"]
                  , typ "member"
                  ]
    result `shouldSatisfy` \res -> complDocumentation res == Just "doc for member\n"

  it "includes declarationType in completions for values" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "exampleValue"
                  ]
    result `shouldSatisfy` \res ->
      complDeclarationType res == Just DeclarationType.Value

  it "includes declarationType in completions for functions" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "exampleFunction"
                  ]
    result `shouldSatisfy` \res ->
      complDeclarationType res == Just DeclarationType.Value

  it "includes declarationType in completions for inferred values" $ do
    ([_, (Right (CompletionResult [ result ]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "exampleInferredString"
                  ]
    result `shouldSatisfy` \res ->
      complDeclarationType res == Just DeclarationType.Value

  it "includes declarationType in completions for operators" $ do
    ([_, (Right (CompletionResult results))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "\\°/"
                  ]
    length results `shouldBe` 2
    results `shouldSatisfy` any (\res ->
      complDeclarationType res == Just DeclarationType.ValueOperator)
    results `shouldSatisfy` any (\res ->
      complDeclarationType res == Just DeclarationType.TypeOperator)

  it "includes declarationType in completions for type constructors with \
      \conflicting names" $ do
    ([_, (Right (CompletionResult results))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "ExampleTypeConstructor"
                  ]
    length results `shouldBe` 2
    results `shouldSatisfy` any (\res ->
        complDeclarationType res == Just DeclarationType.DataConstructor)
    results `shouldSatisfy` any (\res ->
        complDeclarationType res == Just DeclarationType.Type)

  it "includes declarationType in completions for type classes" $ do
    ([_, (Right (CompletionResult [result]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "ExampleClass"
                  ]
    result `shouldSatisfy` \res ->
      complDeclarationType res == Just DeclarationType.TypeClass

  it "includes declarationType in completions for type class members" $ do
    ([_, (Right (CompletionResult [result]))], _) <- Test.inProject $
      Test.runIde [ load ["CompletionSpec"]
                  , typ "exampleMember"
                  ]
    result `shouldSatisfy` \res ->
      complDeclarationType res == Just DeclarationType.Value
