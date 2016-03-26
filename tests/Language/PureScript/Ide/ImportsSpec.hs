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

splitSimpleFile :: (P.ModuleName, [Text], [Import], [Text])
splitSimpleFile = fromRight $ sliceImportSection simpleFile
  where
    fromRight (Right r) = r
    fromRight (Left _) = error "fromRight"

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
    let moduleSkeleton imports =
          Right (P.moduleNameFromString "Main", take 1 simpleFile, imports, drop 2 simpleFile)
    it "finds a simple import" $
      shouldBe (sliceImportSection simpleFile) (moduleSkeleton [preludeImport])

    it "allows multiline import statements" $
      shouldBe
        (sliceImportSection (withImports [ "import Data.Array (head,"
                                         , "                   cons)"
                                         ]))
        (moduleSkeleton [preludeImport, arrayImport])
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
    let simpleFileImports = let (_, _, i, _) = splitSimpleFile in i
        addExplicitImportTest i mn is = prettyPrintImportSection (addExplicitImport' i mn is)
    it "adds an implicit unqualified import" $
      shouldBe
        (addImplicitImport' simpleFileImports (P.moduleNameFromString "Data.Map"))
        [ "import Data.Map"
        , "import Prelude"
        ]
    it "adds an explicit unqualified import" $
      shouldBe
        (addExplicitImportTest (P.Ident "head") (P.moduleNameFromString "Data.Array") simpleFileImports)
        [ "import Data.Array (head)"
        , "import Prelude"
        ]
    let Right (_, _, explicitImports, _) = sliceImportSection (withImports ["import Data.Array (tail)"])
    it "adds an identifier to an explicit import list" $
      shouldBe
        (addExplicitImportTest (P.Ident "head") (P.moduleNameFromString "Data.Array") explicitImports)
        [ "import Data.Array (head, tail)"
        , "import Prelude"
        ]
    it "doesn't add an identifier to an explicit import list if it's already imported" $
      shouldBe
      (addExplicitImportTest (P.Ident "tail") (P.moduleNameFromString "Data.Array") explicitImports)
      [ "import Data.Array (tail)"
      , "import Prelude"
      ]
