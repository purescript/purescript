{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.UsageSpec where

import           Protolude

import qualified Data.Text as Text
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Ide.Test as Test
import qualified Language.PureScript as P
import           Test.Hspec
import           Data.Text.Read (decimal)
import System.FilePath

load :: [Text] -> Command
load = LoadSync . map Test.mn

usage :: P.ModuleName -> Text -> IdeNamespace -> Command
usage mn ident ns = FindUsages mn ident ns

shouldBeUsage :: P.SourceSpan -> (FilePath, Text) -> Expectation
shouldBeUsage usage' (fp, range) =
  let
    [ start, end] = Text.splitOn "-" range
    unsafeReadInt = fst . either (panic "") identity . decimal
    [ startLine, startColumn ] = map unsafeReadInt (Text.splitOn ":" start)
    [ endLine, endColumn ] = map unsafeReadInt (Text.splitOn ":" end)
  in
    do
      fp `shouldBe` P.spanName usage'

      (P.sourcePosLine (P.spanStart usage'), P.sourcePosColumn (P.spanStart usage'))
        `shouldBe`
        (startLine, startColumn)

      (P.sourcePosLine (P.spanEnd usage'), P.sourcePosColumn (P.spanEnd usage'))
        `shouldBe`
        (endLine, endColumn)

spec :: Spec
spec = describe "Finding Usages" $ do
    it "finds a simple usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "usageId" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:11-12:18")
    it "finds a constructor usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "Used" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "8:3-8:9")
    it "finds a constructor alias usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "$%" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "9:5-9:7")
    it "finds a reexported usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Reexport") "toBeReexported" IdeNSValue
                    ]
        -- TODO(Christoph): Interesting parser bug here. It seems the position
        -- of the last token in the file has the wrong ending span
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:19-12:19")
