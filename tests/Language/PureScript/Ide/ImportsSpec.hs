{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.ImportsSpec where

import Test.Hspec
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.PureScript as P
import Language.PureScript.Ide.Imports
import Language.PureScript.Ide.Integration

import System.FilePath

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

preludeImport :: Import
preludeImport = Import (P.moduleNameFromString "Prelude") P.Implicit Nothing

arrayImport :: Import
arrayImport =
  Import
    (P.moduleNameFromString "Data.Array")
    (P.Explicit [P.ValueRef (P.Ident "head"), P.ValueRef (P.Ident "cons")])
    Nothing

listImport :: Import
listImport =
  Import
    (P.moduleNameFromString "Data.List")
    P.Implicit
    (Just (P.moduleNameFromString "List"))

consoleImport :: Import
consoleImport =
  Import
    (P.moduleNameFromString "Control.Monad.Eff.Console")
    (P.Explicit [P.ValueRef (P.Ident "log")])
    (Just (P.moduleNameFromString "Console"))

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
      shouldBe
        (prettyPrintImport' preludeImport)
        "import Prelude"
    it "pretty prints an explicit import" $
      shouldBe
        (prettyPrintImport' arrayImport)
        "import Data.Array (head, cons)"
    it "pretty prints a qualified import" $
      shouldBe
        (prettyPrintImport' listImport)
        "import Data.List as List"
    it "pretty prints a qualified explicit import" $
      shouldBe
        (prettyPrintImport' consoleImport)
        "import Control.Monad.Eff.Console (log) as Console"

  describe "import commands" $ do
    it "adds an implicit unqualified import" $
      let (_, imports, _) = splitSimpleFile
      in
        shouldBe
          (addImplicitImport' imports (P.moduleNameFromString "Data.Map"))
          [ "import Data.Map"
          , "import Prelude"
          , ""
          ]
    it "adds an explicit unqualified import" $
      let (_, imports, _) = splitSimpleFile
      in
        shouldBe
          (addExplicitImport' (P.Ident "head") (P.moduleNameFromString "Data.Array") imports)
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
  beforeAll_ setup $ afterAll_ teardown $
    describe "Integration Tests:" $ do
      it "adds an explicit unqualified import" $ do
        pdir <- projectDirectory
        let sourceFp = pdir </> "src" </> "ImportsSpec.purs"
            outFp = pdir </> "src" </> "ImportsSpecOut.tmp"
        _ <- addImport "exportedFunction" sourceFp outFp
        res <- TIO.readFile outFp
        shouldBe
          (T.lines res)
          [ "module ImportsSpec where"
          , ""
          , "import ImportsSpec1 (exportedFunction)"
          , "import Main (id)"
          , ""
          , "myId = id"
          ]

setup :: IO ()
setup = do
  deleteOutputFolder
  s <- compileTestProject
  unless s $ fail "Failed to compile .purs sources"
  _ <- startServer
  _ <- loadModuleWithDeps "ImportsSpec"
  _ <- loadModuleWithDeps "ImportsSpec1"
  return ()

teardown :: IO ()
teardown = quitServer
