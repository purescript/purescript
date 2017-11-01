{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Language.PureScript.Ide.UsagesSpec where

import           Protolude hiding (moduleName)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Control.Lens
import           Language.PureScript.Ide.Usages (Usage(..), collectUsages, insertUsage)
import qualified Language.PureScript.Ide.Command as Command
import qualified Language.PureScript.Ide.Filter as Filter
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Ide.Test as Test
import qualified Language.PureScript as P
import           Test.Hspec
import           Data.Text.Read (decimal)

testModule :: P.Module
testModule =
  either (panic "") snd $ P.parseModuleFromFile (const "") $ ("" :: [Char], ) $ T.unlines
    [ "module Test where"
    , ""
    , "import Prelude"
    , ""
    , "import Data.Array (filter)"
    , "import Data.List as List"
    , "import Globals (globalList)"
    , ""
    , "id x = x"
    , "localFn y = filter isEven y"
    , "rofl = map localFn globalList"
    , "anotherTest = List.map localFn globalList"
    ]

shouldBeUsage :: Text -> (P.ModuleName, P.SourceSpan) -> Expectation
shouldBeUsage test usage =
  let
    [ moduleName, start, end] = T.splitOn "-" test
    unsafeReadInt = fst . either (panic "") identity . decimal
    [ startLine, startColumn ] = map unsafeReadInt (T.splitOn ":" start)
    [ endLine, endColumn ] = map unsafeReadInt (T.splitOn ":" end)
  in
    do
      moduleName `shouldBe` P.runModuleName (fst usage)

      (startLine, startColumn)
        `shouldBe`
          ( P.sourcePosLine (P.spanStart (snd usage))
          , P.sourcePosColumn (P.spanStart (snd usage)))
      (endLine, endColumn)
        `shouldBe`
          ( P.sourcePosLine (P.spanEnd (snd usage))
          , P.sourcePosColumn (P.spanEnd (snd usage)))

shouldFindUsage :: [Usage] -> (IdeDeclarationId, P.SourcePos, P.SourcePos) -> Expectation
shouldFindUsage us (id, start, end) =
  let
    matchingIds = filter (\u -> id == usageOriginId u) us
  in
    shouldSatisfy matchingIds
    (any (\u ->
      start == P.spanStart (usageSiteLocation u)
      && end == P.spanEnd (usageSiteLocation u)))

findByIdCommand :: P.ModuleName -> IdeNamespace -> Text -> Command.Command
findByIdCommand mn ns id =
  Command.Query
    [ Filter.moduleFilter [mn]
    , Filter.namespaceFilter (pure ns)
    , Filter.equalityFilter id
    ] Nothing

spec :: Spec
spec = do
  describe "collect usages" $ do
    let usages = collectUsages testModule
    it "should find an explicitly imported value" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Data.Array") IdeNSValue "filter"
        , P.SourcePos 10 13
        , P.SourcePos 10 19)
    it "should find an implicitly imported value" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Prelude") IdeNSValue "map"
        , P.SourcePos 11 8
        , P.SourcePos 11 11)
    it "should find a qualified usage" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Data.List") IdeNSValue "map"
        , P.SourcePos 12 15
        , P.SourcePos 12 23)
    it "should find a locally defined value" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Test") IdeNSValue "localFn"
        , P.SourcePos 11 12
        , P.SourcePos 11 19)
    it "should find a usage in the import section" $
      usages `shouldFindUsage`
        ( IdeDeclarationId (Test.mn "Data.Array") IdeNSValue "filter"
        , P.SourcePos 5 20
        , P.SourcePos 5 26)
  describe "resolving usages" $ do
    it "resolves a reexported value usage" $ do
      let
        ms = Map.fromList
          [ (Test.mn "A", [Test.ideValue "hello" Nothing])
          , (Test.mn "B", [Test.ideValue "hello" Nothing `Test.annExp` "A"])
          ]
        usage = Usage
          { usageOriginId = IdeDeclarationId (Test.mn "B") IdeNSValue "hello"
          , usageSiteModule = Test.mn "C"
          , usageSiteLocation = Test.ss 1 1
          }
        resolved = insertUsage usage ms
      resolved ^. ix (Test.mn "A") . ix 0 . idaAnnotation . annUsages `shouldBe` Just [ (Test.mn "C", Test.ss 1 1) ]
  describe "retrieving usages" $ do
    it "returns a simple value usage" $ do
      ([_, Right (QueryResult [(_, [da])])], _) <- Test.inProject $
        Test.runIde [Command.LoadSync [], findByIdCommand (Test.mn "RebuildSpecDep") IdeNSValue "dep"]
      let Just usages = da ^. idaAnnotation . annUsages
      let cases = zip ["RebuildSpecWithDeps-3:24-3:27", "RebuildSpecWithDeps-5:5-5:5"] usages
      for_ cases (uncurry shouldBeUsage)
