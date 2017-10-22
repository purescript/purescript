{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Language.PureScript.Ide.UsagesSpec where

import           Protolude
import qualified Data.Text as T
import           Language.PureScript.Ide.Usages (Usage(..), collectUsages)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Test as Test
import qualified Language.PureScript as P
import           Test.Hspec

testModule :: P.Module
testModule =
  either (panic "") snd $ P.parseModuleFromFile (const "") $ ("" :: [Char], ) $ T.unlines
    [ "module Test where"
    , ""
    , "import Prelude"
    , ""
    , "import Data.Array (filter)"
    , "import Globals (globalList)"
    , ""
    , "id x = x"
    , "localFn y = filter isEven y"
    , "rofl = map localFn globalList"
    ]

shouldFindUsage :: [Usage] -> (IdeDeclarationId, P.SourcePos, P.SourcePos) -> Expectation
shouldFindUsage us (id, start, end) =
  let
    matchingIds = filter (\u -> id == usageOriginId u) us
  in
    shouldSatisfy matchingIds
    (any (\u ->
      start == P.spanStart (usageSiteLocation u)
      && end == P.spanEnd (usageSiteLocation u)))

spec :: Spec
spec = do
  describe "collect usages" $ do
    let usages = collectUsages testModule
    it "should find an explicitly imported value" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Data.Array") IdeNSValue "filter"
        , P.SourcePos 9 13
        , P.SourcePos 9 19)
    it "should find an implicitly imported value" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Prelude") IdeNSValue "map"
        , P.SourcePos 10 8
        , P.SourcePos 10 11)
    it "should find a locally defined value" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Test") IdeNSValue "localFn"
        , P.SourcePos 10 12
        , P.SourcePos 10 19)
    it "should find a usage in the import section" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Data.Array") IdeNSValue "filter"
        , P.SourcePos 5 20
        , P.SourcePos 5 26)
