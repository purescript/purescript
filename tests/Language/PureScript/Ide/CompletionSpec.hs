{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.CompletionSpec where

import           Protolude hiding (to, from, (&))
import           Control.Lens
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Completion.Formatter
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Test as Test
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P
import           Test.Hspec

type Module = (P.ModuleName, [IdeDeclarationAnn])

moduleB :: (Text, [IdeDeclarationAnn])
moduleB = ("ModuleB",
           [ ideValue "sfValueB" Nothing
           , ideSynonym "SFTypeB" P.tyString P.kindType
           , ideType "SFDataB" Nothing
           ])

moduleC :: (Text, [IdeDeclarationAnn])
moduleC = ("ModuleC",
           [ ideValue "sfValueB" Nothing `annExp` "ModuleB"
           , ideSynonym "SFTypeC" P.tyString P.kindType
           ])

moduleA :: (Text, [IdeDeclarationAnn])
moduleA = ("ModuleA",
           [ ideValue "sfValue" Nothing
           , ideSynonym "SFType" P.tyString P.kindType
           , ideType "SFData" Nothing
           , ideDtor "SFOne" "SFData" Nothing
           , ideDtor "SFTwo" "SFData" Nothing
           , ideDtor "SFThree" "SFData" Nothing
           , ideTypeClass "SFClass" P.kindType []
           , ideValueOp "<$>" (P.Qualified Nothing (Left "")) 0 Nothing Nothing
           , ideTypeOp "~>" (P.Qualified Nothing "") 0 Nothing Nothing
           ])
m :: Text -> P.ModuleName
m = P.moduleNameFromString

moduleBCompletions :: [Completion]
moduleBCompletions = runFormatter defaultFormatter $ map (\decl -> Match (m "ModuleB", decl)) (snd moduleB)

getCompletions :: [Text] -> CompletionFormatter -> IO [Completion]
getCompletions modules formatter = Test.inProject $ do
  ([Right (CompletionResult cs)], _) <-
    Test.runIde' Test.defConfig ideState [Complete [moduleFilter (map m modules)] mempty formatter Nothing]
  pure cs
  where
    ideState = emptyIdeState `s3` [moduleA, moduleB, moduleC]

shouldBeEqualSorted :: (Show a, Ord a) => [a] -> [a] -> Expectation
shouldBeEqualSorted = shouldBe `on` sort

spec :: Spec
spec = do
  describe "Filtering by FilePath" $ do
    it "should filter by the imported modules" $ do
      cs <- getCompletions ["ModuleB"] defaultFormatter
      cs `shouldBeEqualSorted` moduleBCompletions
    it "should omit reexported definitions that are already in scope and list them as reexported" $ do
      cs <- getCompletions ["ModuleB", "ModuleC"] (CompletionFormatter Flatten)
      let cCompletions = runFormatter (CompletionFormatter Flatten) [Match (m "ModuleC", ideSynonym "SFTypeC" P.tyString P.kindType)]
          bCompletions = moduleBCompletions
            & map (\c -> c {complReexports = Just []})
            & ix 0 %~ \c -> c {complReexports = Just ["ModuleC"]}
      cs `shouldBeEqualSorted` (bCompletions ++ cCompletions)
