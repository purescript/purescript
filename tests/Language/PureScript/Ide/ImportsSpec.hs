{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.ImportsSpec where

import           Data.Maybe                          (fromJust)
import           Data.Text                           (Text)
import qualified Language.PureScript                 as P
import           Language.PureScript.Ide.Imports
import           Test.Hspec

simpleFile :: [Text]
simpleFile =
  [ "module Main where"
  , "import Prelude"
  , ""
  , "myFunc x y = x + y"
  ]

splitSimpleFile :: ([Text], [Import], [Text])
splitSimpleFile = sliceImportSection simpleFile

withImports :: [Text] -> [Text]
withImports is =
  take 2 simpleFile ++ is ++ drop 2 simpleFile

testParseImport :: Text -> Import
testParseImport = fromJust . parseImport

preludeImport, arrayImport, listImport, consoleImport, maybeImport :: Import
preludeImport = testParseImport "import Prelude"
arrayImport = testParseImport "import Data.Array (head, cons)"
listImport = testParseImport "import Data.List as List"
consoleImport = testParseImport "import Control.Monad.Eff.Console (log) as Console"
maybeImport = testParseImport "import Data.Maybe (Maybe(Just))"

spec :: Spec
spec = do
  describe "determining the importsection" $ do
    it "finds a simple import" $
      shouldBe
        (sliceImportSection simpleFile)
        (take 1 simpleFile, [preludeImport], drop 3 simpleFile)

    it "allows multiline import statements" $
      shouldBe
        (sliceImportSection (withImports [ "import Data.Array (head,"
                                         , "                   cons)"
                                         ]))
        (take 1 simpleFile, [preludeImport, arrayImport], drop 3 simpleFile)
  describe "pretty printing imports" $ do
    it "pretty prints a simple import" $
      shouldBe (prettyPrintImport' preludeImport) "import Prelude"
    it "pretty prints an explicit import" $
      shouldBe (prettyPrintImport' arrayImport) "import Data.Array (head, cons)"
    it "pretty prints a qualified import" $
      shouldBe (prettyPrintImport' listImport) "import Data.List as List"
    it "pretty prints a qualified explicit import" $
      shouldBe (prettyPrintImport' consoleImport) "import Control.Monad.Eff.Console (log) as Console"
    it "pretty prints an import with a datatype (and PositionedRef's for the dtors)" $
      shouldBe (prettyPrintImport' maybeImport) "import Data.Maybe (Maybe(Just))"

  describe "import commands" $ do
    let simpleFileImports = let (_, i, _) = splitSimpleFile in i
    it "adds an implicit unqualified import" $
      shouldBe
        (addImplicitImport' simpleFileImports (P.moduleNameFromString "Data.Map"))
        [ "import Data.Map"
        , "import Prelude"
        , ""
        ]
    it "adds an explicit unqualified import" $
      shouldBe
        (addExplicitImport' (P.Ident "head") (P.moduleNameFromString "Data.Array") simpleFileImports)
        [ "import Data.Array (head)"
        , "import Prelude"
        , ""
        ]
    it "adds an identifier to an explicit import list" $
      let (_, imports, _) = sliceImportSection (withImports ["import Data.Array (tail)"])
      in
        shouldBe
          (addExplicitImport' (P.Ident "head") (P.moduleNameFromString "Data.Array") imports)
          [ "import Data.Array (head, tail)"
          , "import Prelude"
          , ""
          ]
