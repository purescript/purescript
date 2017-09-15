{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.ImportsSpec where

import           Protolude hiding (moduleName)
import           Data.Maybe                      (fromJust)

import qualified Language.PureScript             as P
import           Language.PureScript.Ide.Command as Command
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Imports
import qualified Language.PureScript.Ide.Test as Test
import           Language.PureScript.Ide.Types
import           System.FilePath
import           Test.Hspec

noImportsFile :: [Text]
noImportsFile =
  [ "module Main where"
  , ""
  , "myFunc x y = x + y"
  ]

simpleFile :: [Text]
simpleFile =
  [ "module Main where"
  , "import Prelude"
  , ""
  , "myFunc x y = x + y"
  ]

hidingFile :: [Text]
hidingFile =
  [ "module Main where"
  , "import Prelude"
  , "import Data.Maybe hiding (maybe)"
  , ""
  , "myFunc x y = x + y"
  ]

syntaxErrorFile :: [Text]
syntaxErrorFile =
  [ "module Main where"
  , "import Prelude"
  , ""
  , "myFunc ="
  ]

testSliceImportSection :: [Text] -> (P.ModuleName, [Text], [Import], [Text])
testSliceImportSection = fromRight . sliceImportSection
  where
    fromRight = fromJust . rightToMaybe

withImports :: [Text] -> [Text]
withImports is =
  take 2 simpleFile ++ [""] ++ is ++ drop 2 simpleFile

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
    it "slices a file without imports and adds a newline after the module declaration" $
      shouldBe (sliceImportSection noImportsFile)
          (Right (P.moduleNameFromString "Main", take 1 noImportsFile ++ [""], [], drop 1 noImportsFile))

    it "handles a file with syntax errors just fine" $
      shouldBe (sliceImportSection syntaxErrorFile)
      (Right (P.moduleNameFromString "Main", take 1 syntaxErrorFile, [preludeImport], drop 2 syntaxErrorFile))

    it "finds a simple import" $
      shouldBe (sliceImportSection simpleFile) (moduleSkeleton [preludeImport])

    it "allows multiline import statements" $
      shouldBe
        (sliceImportSection (withImports [ "import Data.Array (head,"
                                         , "                   cons)"
                                         ]))
        (moduleSkeleton [preludeImport, arrayImport])
    it "allows multiline import statements with hanging parens" $
      shouldBe
        (sliceImportSection (withImports [ "import Data.Array ("
                                         , "  head,"
                                         , "  cons"
                                         , ")"
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
    let simpleFileImports = let (_, _, i, _) = testSliceImportSection simpleFile in i
        hidingFileImports = let (_, _, i, _) = testSliceImportSection hidingFile in i
        addValueImport i mn q is =
          prettyPrintImportSection (addExplicitImport' (_idaDeclaration (Test.ideValue i Nothing)) mn q is)
        addOpImport op mn q is =
          prettyPrintImportSection (addExplicitImport' (_idaDeclaration (Test.ideValueOp op (P.Qualified q (Left "")) 2 Nothing Nothing)) mn q is)
        addDtorImport i t mn q is =
          prettyPrintImportSection (addExplicitImport' (_idaDeclaration (Test.ideDtor i t Nothing)) mn q is)
        addTypeImport i mn q is =
          prettyPrintImportSection (addExplicitImport' (_idaDeclaration (Test.ideType i Nothing [])) mn q is)
        addKindImport i mn q is =
          prettyPrintImportSection (addExplicitImport' (_idaDeclaration (Test.ideKind i)) mn q is)
        qualify s = Just (Test.mn s)
    it "adds an implicit unqualified import to a file without any imports" $
      shouldBe
        (addImplicitImport' [] (P.moduleNameFromString "Data.Map"))
        ["import Data.Map"]
    it "adds an implicit unqualified import" $
      shouldBe
        (addImplicitImport' simpleFileImports (P.moduleNameFromString "Data.Map"))
        [ "import Data.Map"
        , "import Prelude"
        ]
    it "adds a qualified import" $
      shouldBe
        (addQualifiedImport' simpleFileImports (Test.mn "Data.Map") (Test.mn "Map"))
        [ "import Prelude"
        , ""
        , "import Data.Map as Map"
        ]
    it "adds a qualified import and maintains proper grouping for implicit hiding imports" $
      shouldBe
        (addQualifiedImport' hidingFileImports (Test.mn "Data.Map") (Test.mn "Map"))
        [ "import Data.Maybe hiding (maybe)"
        , "import Prelude"
        , ""
        , "import Data.Map as Map"
        ]
    it "adds an explicit unqualified import to a file without any imports" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") Nothing [])
        ["import Data.Array (head)"]
    it "adds an explicit qualified import to a file without any imports" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") (qualify "Array") [])
        ["import Data.Array (head) as Array"]
    it "adds an explicit unqualified import" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") Nothing simpleFileImports)
        [ "import Prelude"
        , ""
        , "import Data.Array (head)"
        ]
    it "adds an explicit qualified import" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") (qualify "Array") simpleFileImports)
        [ "import Prelude"
        , ""
        , "import Data.Array (head) as Array"
        ]
    it "doesn't add an import if the containing module is imported implicitly" $
      shouldBe
      (addValueImport "const" (P.moduleNameFromString "Prelude") Nothing simpleFileImports)
      ["import Prelude"]
    let Right (_, _, qualifiedImports, _) = sliceImportSection (withImports ["import Data.Array as Array"])
    it "doesn't add a qualified explicit import if the containing module is imported qualified" $
      shouldBe
      (addValueImport "length" (P.moduleNameFromString "Data.Array") (qualify "Array") qualifiedImports)
      ["import Prelude"
      , ""
      , "import Data.Array as Array"]
    let Right (_, _, explicitImports, _) = sliceImportSection (withImports ["import Data.Array (tail)"])
    it "adds an identifier to an explicit import list" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") Nothing explicitImports)
        [ "import Prelude"
        , ""
        , "import Data.Array (head, tail)"
        ]
    let Right (_, _, explicitQualImports, _) = sliceImportSection (withImports ["import Data.Array (tail) as Array"])
    it "adds an identifier to an explicit qualified import list" $
      shouldBe
        (addValueImport "head" (P.moduleNameFromString "Data.Array") (qualify "Array") explicitQualImports)
        [ "import Prelude"
        , ""
        , "import Data.Array (head, tail) as Array"
        ]
    it "adds a kind to an explicit import list" $
      shouldBe
        (addKindImport "Effect" (P.moduleNameFromString "Control.Monad.Eff") Nothing simpleFileImports)
        [ "import Prelude"
        , ""
        , "import Control.Monad.Eff (kind Effect)"
        ]
    it "adds a kind to an explicit qualified import list" $
      shouldBe
        (addKindImport "Effect" (P.moduleNameFromString "Control.Monad.Eff") (qualify "Eff") simpleFileImports)
        [ "import Prelude"
        , ""
        , "import Control.Monad.Eff (kind Effect) as Eff"
        ]
    it "adds an operator to an explicit import list" $
      shouldBe
        (addOpImport "<~>" (P.moduleNameFromString "Data.Array") Nothing explicitImports)
        [ "import Prelude"
        , ""
        , "import Data.Array (tail, (<~>))"
        ]
    it "adds an operator to an explicit qualified import list" $
      shouldBe
        (addOpImport "<~>" (P.moduleNameFromString "Data.Array") (qualify "Array") explicitQualImports)
        [ "import Prelude"
        , ""
        , "import Data.Array (tail, (<~>)) as Array"
        ]
    it "adds a type with constructors without automatically adding an open import of said constructors " $
        shouldBe
          (addTypeImport "Maybe" (P.moduleNameFromString "Data.Maybe") Nothing simpleFileImports)
          [ "import Prelude"
          , ""
          , "import Data.Maybe (Maybe)"
          ]
    it "adds the type for a given DataConstructor" $
        shouldBe
          (addDtorImport "Just" "Maybe" (P.moduleNameFromString "Data.Maybe") Nothing simpleFileImports)
          [ "import Prelude"
          , ""
          , "import Data.Maybe (Maybe(..))"
          ]
    it "adds the type for a given DataConstructor qualified" $
        shouldBe
          (addDtorImport "Just" "Maybe" (P.moduleNameFromString "Data.Maybe") (qualify "M") simpleFileImports)
          [ "import Prelude"
          , ""
          , "import Data.Maybe (Maybe(..)) as M"
          ]
    it "adds a dataconstructor to an existing type import" $ do
      let Right (_, _, typeImports, _) = sliceImportSection (withImports ["import Data.Maybe (Maybe)"])
      shouldBe
        (addDtorImport "Just" "Maybe" (P.moduleNameFromString "Data.Maybe") Nothing typeImports)
        [ "import Prelude"
        , ""
        , "import Data.Maybe (Maybe(..))"
        ]
    it "adds a dataconstructor to an existing qualified type import" $ do
      let Right (_, _, typeImports, _) = sliceImportSection (withImports ["import Data.Maybe (Maybe) as M"])
      shouldBe
        (addDtorImport "Just" "Maybe" (P.moduleNameFromString "Data.Maybe") (qualify "M") typeImports)
        [ "import Prelude"
        , ""
        , "import Data.Maybe (Maybe(..)) as M"
        ]
    it "doesn't add a dataconstructor to an existing type import with open dtors" $ do
      let Right (_, _, typeImports, _) = sliceImportSection (withImports ["import Data.Maybe (Maybe(..))"])
      shouldBe
        (addDtorImport "Just" "Maybe" (P.moduleNameFromString "Data.Maybe") Nothing typeImports)
        [ "import Prelude"
        , ""
        , "import Data.Maybe (Maybe(..))"
        ]
    it "doesn't add an identifier to an explicit import list if it's already imported" $
      shouldBe
      (addValueImport "tail" (P.moduleNameFromString "Data.Array") Nothing explicitImports)
      [ "import Prelude"
      , ""
      , "import Data.Array (tail)"
      ]
    it "doesn't add an identifier to an explicit qualified import list if it's already imported qualified" $
      shouldBe
      (addValueImport "tail" (P.moduleNameFromString "Data.Array") (qualify "Array") explicitQualImports)
      [ "import Prelude"
      , ""
      , "import Data.Array (tail) as Array"
      ]

  describe "explicit import sorting" $ do
    -- given some basic import skeleton
    let Right (_, _, baseImports, _) = sliceImportSection $ withImports ["import Control.Monad (ap)"]
        moduleName = (P.moduleNameFromString "Control.Monad")
        addImport imports import' = addExplicitImport' import' moduleName Nothing imports
        valueImport ident = _idaDeclaration (Test.ideValue ident Nothing)
        typeImport name = _idaDeclaration (Test.ideType name Nothing [])
        classImport name = _idaDeclaration (Test.ideTypeClass name P.kindType [])
        dtorImport name typeName = _idaDeclaration (Test.ideDtor name typeName Nothing)
        -- expect any list of provided identifiers, when imported, to come out as specified
        expectSorted imports expected = shouldBe
          (ordNub $ map
            (prettyPrintImportSection . foldl addImport baseImports)
            (permutations imports))
          [expected]
    it "sorts class" $
      expectSorted (map classImport ["Applicative", "Bind"])
        ["import Prelude", "", "import Control.Monad (class Applicative, class Bind, ap)"]
    it "sorts value" $
      expectSorted (map valueImport ["unless", "where"])
        ["import Prelude", "", "import Control.Monad (ap, unless, where)"]
    it "sorts type, value" $
      expectSorted
        ((map valueImport ["unless", "where"]) ++ (map typeImport ["Foo", "Bar"]))
        ["import Prelude", "", "import Control.Monad (Bar, Foo, ap, unless, where)"]
    it "sorts class, type, value" $
      expectSorted
        ((map valueImport ["unless", "where"]) ++ (map typeImport ["Foo", "Bar"]) ++ (map classImport ["Applicative", "Bind"]))
        ["import Prelude", "", "import Control.Monad (class Applicative, class Bind, Bar, Foo, ap, unless, where)"]
    it "sorts types with constructors, using open imports for the constructors" $
      expectSorted
        -- the imported names don't actually have to exist!
        (map (uncurry dtorImport) [("Just", "Maybe"), ("Nothing", "Maybe"), ("SomeOtherConstructor", "SomeDataType")])
        ["import Prelude", "", "import Control.Monad (Maybe(..), SomeDataType(..), ap)"]
  describe "importing from a loaded IdeState" importFromIdeState

implImport :: Text -> Command
implImport mn =
  Command.Import ("src" </> "ImportsSpec.purs") Nothing [] (Command.AddImplicitImport (Test.mn mn))

addExplicitImport :: Text -> Command
addExplicitImport i =
  Command.Import ("src" </> "ImportsSpec.purs") Nothing [] (Command.AddImportForIdentifier i Nothing)

importShouldBe :: [Text] -> [Text] -> Expectation
importShouldBe res importSection =
  res `shouldBe` [ "module ImportsSpec where" , ""] ++ importSection ++ [ "" , "myId x = x"]

runIdeLoaded :: Command -> IO (Either IdeError Success)
runIdeLoaded c = do
  ([_, result], _) <- Test.inProject $ Test.runIde [Command.LoadSync [] , c]
  pure result

importFromIdeState :: Spec
importFromIdeState = do
  it "adds an implicit import" $ do
    Right (MultilineTextResult result) <-
      runIdeLoaded (implImport "ImportsSpec1")
    result `importShouldBe` [ "import ImportsSpec1" ]
  it "adds an explicit unqualified import" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "exportedFunction")
    result `importShouldBe` [ "import ImportsSpec1 (exportedFunction)" ]
  it "adds an explicit unqualified import (type)" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "MyType")
    result `importShouldBe` [ "import ImportsSpec1 (MyType)" ]
  it "adds an explicit unqualified import (parameterized type)" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "MyParamType")
    result `importShouldBe` [ "import ImportsSpec1 (MyParamType)" ]
  it "adds an explicit unqualified import (typeclass)" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "ATypeClass")
    result `importShouldBe` [ "import ImportsSpec1 (class ATypeClass)" ]
  it "adds an explicit unqualified import (dataconstructor)" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "MyJust")
    result `importShouldBe` [ "import ImportsSpec1 (MyMaybe(..))" ]
  it "adds an explicit unqualified import (newtype)" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "MyNewtype")
    result `importShouldBe` [ "import ImportsSpec1 (MyNewtype(..))" ]
  it "adds an explicit unqualified import (typeclass member function)" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "typeClassFun")
    result `importShouldBe` [ "import ImportsSpec1 (typeClassFun)" ]
  it "doesn't add a newtypes constructor if only the type is exported" $ do
    Right (MultilineTextResult result) <-
      runIdeLoaded (addExplicitImport "OnlyTypeExported")
    result `importShouldBe` [ "import ImportsSpec1 (OnlyTypeExported)" ]
  it "doesn't add an import if the identifier is defined in the module itself" $ do
    Right (MultilineTextResult result) <- runIdeLoaded (addExplicitImport "myId")
    result `importShouldBe` []
  it "responds with an error if it's undecidable whether we want a type or constructor" $ do
    result <- runIdeLoaded (addExplicitImport "SpecialCase")
    result `shouldSatisfy` isLeft
  it "responds with an error if the identifier cannot be found and doesn't \
     \write to the output file" $ do
    result <- runIdeLoaded (addExplicitImport "doesnExist")
    result `shouldSatisfy` isLeft
