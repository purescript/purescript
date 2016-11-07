{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.ImportsSpec where

import           Protolude
import           Data.Maybe                      (fromJust)

import qualified Language.PureScript             as P
import           Language.PureScript.Ide.Imports
import           Language.PureScript.Ide.Types
import           Test.Hspec

simpleFile :: [Text]
simpleFile =
  [ "module Main where"
  , "import Prelude"
  , ""
  , "myFunc x y = x + y"
  ]

splitSimpleFile :: (P.ModuleName, [Text], [Import], [Text])
splitSimpleFile = fromRight (sliceImportSection simpleFile)
  where
    fromRight = fromJust . rightToMaybe
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

wildcard :: P.Type
wildcard = P.TypeWildcard $ P.SourceSpan "" (P.SourcePos 0 0) (P.SourcePos 0 0)

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
        addValueImport i mn is =
          prettyPrintImportSection (addExplicitImport' (IdeDeclValue (IdeValue (P.Ident i) wildcard)) mn is)
        addOpImport op mn is =
          prettyPrintImportSection (addExplicitImport' (IdeDeclValueOperator (IdeValueOperator op (P.Qualified Nothing (Left (P.Ident ""))) 2 P.Infix Nothing)) mn is)
        addDtorImport i t mn is =
          prettyPrintImportSection (addExplicitImport' (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName i) t wildcard)) mn is)
        addTypeImport i mn is =
          prettyPrintImportSection (addExplicitImport' (IdeDeclType (IdeType (P.ProperName i) P.Star)) mn is)
    it "adds an implicit unqualified import" $
      shouldBe
        (addImplicitImport' simpleFileImports (P.moduleNameFromString "Data.Map"))
        [ "import Prelude"
        , "import Data.Map"
        ]
    it "adds an explicit unqualified import" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") simpleFileImports)
        [ "import Prelude"
        , "import Data.Array (head)"
        ]
    it "doesn't add an import if the containing module is imported implicitly" $
      shouldBe
      (addValueImport "const" (P.moduleNameFromString "Prelude") simpleFileImports)
      ["import Prelude"]
    let Right (_, _, explicitImports, _) = sliceImportSection (withImports ["import Data.Array (tail)"])
    it "adds an identifier to an explicit import list" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") explicitImports)
        [ "import Prelude"
        , "import Data.Array (head, tail)"
        ]
    it "adds an operator to an explicit import list" $
      shouldBe
        (addOpImport (P.OpName "<~>") (P.moduleNameFromString "Data.Array") explicitImports)
        [ "import Prelude"
        , "import Data.Array (tail, (<~>))"
        ]
    it "adds a type with constructors without automatically adding an open import of said constructors " $
        shouldBe
          (addTypeImport "Maybe" (P.moduleNameFromString "Data.Maybe") simpleFileImports)
          [ "import Prelude"
          , "import Data.Maybe (Maybe)"
          ]
    it "adds the type for a given DataConstructor" $
        shouldBe
          (addDtorImport "Just" (P.ProperName "Maybe") (P.moduleNameFromString "Data.Maybe") simpleFileImports)
          [ "import Prelude"
          , "import Data.Maybe (Maybe(..))"
          ]
    it "adds a dataconstructor to an existing type import" $ do
      let Right (_, _, typeImports, _) = sliceImportSection (withImports ["import Data.Maybe (Maybe)"])
      shouldBe
        (addDtorImport "Just" (P.ProperName "Maybe") (P.moduleNameFromString "Data.Maybe") typeImports)
        [ "import Prelude"
        , "import Data.Maybe (Maybe(..))"
        ]
    it "doesn't add a dataconstructor to an existing type import with open dtors" $ do
      let Right (_, _, typeImports, _) = sliceImportSection (withImports ["import Data.Maybe (Maybe(..))"])
      shouldBe
        (addDtorImport "Just" (P.ProperName "Maybe") (P.moduleNameFromString "Data.Maybe") typeImports)
        [ "import Prelude"
        , "import Data.Maybe (Maybe(..))"
        ]
    it "doesn't add an identifier to an explicit import list if it's already imported" $
      shouldBe
      (addValueImport "tail" (P.moduleNameFromString "Data.Array") explicitImports)
      [ "import Prelude"
      , "import Data.Array (tail)"
      ]

  describe "explicit import sorting" $ do
    -- given some basic import skeleton
    let Right (_, _, baseImports, _) = sliceImportSection $ withImports ["import Control.Monad (ap)"]
        moduleName = (P.moduleNameFromString "Control.Monad")
        addImport imports import' = addExplicitImport' import' moduleName imports
        valueImport ident = (IdeDeclValue (IdeValue (P.Ident ident) wildcard))
        typeImport name = (IdeDeclType (IdeType (P.ProperName name) P.Star))
        classImport name = (IdeDeclTypeClass (P.ProperName name))
        dtorImport name typeName = (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName name) (P.ProperName typeName) wildcard))
        -- expect any list of provided identifiers, when imported, to come out as specified
        expectSorted imports expected = shouldBe
          (ordNub $ map
            (prettyPrintImportSection . foldl addImport baseImports)
            (permutations imports))
          [expected]
    it "sorts class" $
      expectSorted (map classImport ["Applicative", "Bind"])
        ["import Prelude", "import Control.Monad (class Applicative, class Bind, ap)"]
    it "sorts value" $
      expectSorted (map valueImport ["unless", "where"])
        ["import Prelude", "import Control.Monad (ap, unless, where)"]
    it "sorts type, value" $
      expectSorted
        ((map valueImport ["unless", "where"]) ++ (map typeImport ["Foo", "Bar"]))
        ["import Prelude", "import Control.Monad (Bar, Foo, ap, unless, where)"]
    it "sorts class, type, value" $
      expectSorted
        ((map valueImport ["unless", "where"]) ++ (map typeImport ["Foo", "Bar"]) ++ (map classImport ["Applicative", "Bind"]))
        ["import Prelude", "import Control.Monad (class Applicative, class Bind, Bar, Foo, ap, unless, where)"]
    it "sorts types with constructors, using open imports for the constructors" $
      expectSorted
        -- the imported names don't actually have to exist!
        (map (uncurry dtorImport) [("Just", "Maybe"), ("Nothing", "Maybe"), ("SomeOtherConstructor", "SomeDataType")])
        ["import Prelude", "import Control.Monad (Maybe(..), SomeDataType(..), ap)"]
