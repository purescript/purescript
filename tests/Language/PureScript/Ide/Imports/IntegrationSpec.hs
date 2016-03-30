{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.Imports.IntegrationSpec where

import           Control.Monad
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TIO
import qualified Language.PureScript.Ide.Integration as Integration
import           Test.Hspec

import           System.Directory
import           System.FilePath

setup :: IO ()
setup = do
  Integration.deleteOutputFolder
  s <- Integration.compileTestProject
  unless s $ fail "Failed to compile .purs sources"
  Integration.quitServer -- kill a eventually running psc-ide-server instance
  _ <- Integration.startServer
  mapM_ Integration.loadModuleWithDeps ["ImportsSpec", "ImportsSpec1"]

teardown :: IO ()
teardown = Integration.quitServer

withSupportFiles :: (FilePath -> FilePath -> IO a) -> IO ()
withSupportFiles test = do
  pdir <- Integration.projectDirectory
  let sourceFp = pdir </> "src" </> "ImportsSpec.purs"
      outFp = pdir </> "src" </> "ImportsSpecOut.tmp"
  Integration.deleteFileIfExists outFp
  void $ test sourceFp outFp

outputFileShouldBe :: [Text] -> IO ()
outputFileShouldBe expectation = do
  outFp <- (</> "src" </> "ImportsSpecOut.tmp") <$> Integration.projectDirectory
  outRes <- TIO.readFile outFp
  shouldBe (T.lines outRes) expectation

spec :: Spec
spec = beforeAll_ setup $ afterAll_ teardown $ describe "Adding imports" $ do
  let
    sourceFileSkeleton :: [Text] -> [Text]
    sourceFileSkeleton importSection =
      [ "module ImportsSpec where" , ""] ++ importSection ++ [ "" , "myId = id"]
  it "adds an implicit import" $ do
    withSupportFiles (Integration.addImplicitImport "Prelude")
    outputFileShouldBe (sourceFileSkeleton
                        [ "import Prelude"
                        , "import Main (id)"
                        ])
  it "adds an explicit unqualified import" $ do
    withSupportFiles (Integration.addImport "exportedFunction")
    outputFileShouldBe (sourceFileSkeleton
                        [ "import ImportsSpec1 (exportedFunction)"
                        , "import Main (id)"
                        ])
  it "adds an explicit unqualified import (type)" $ do
    withSupportFiles (Integration.addImport "MyType")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (MyType)"
                                           , "import Main (id)"
                                           ])
  it "adds an explicit unqualified import (parameterized type)" $ do
    withSupportFiles (Integration.addImport "MyParamType")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (MyParamType)"
                                           , "import Main (id)"
                                           ])
  it "adds an explicit unqualified import (typeclass)" $ do
    withSupportFiles (Integration.addImport "ATypeClass")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (class ATypeClass)"
                                           , "import Main (id)"])
  it "adds an explicit unqualified import (dataconstructor)" $ do
    withSupportFiles (Integration.addImport "MyJust")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (MyMaybe(MyJust))"
                                           , "import Main (id)"])
  it "adds an explicit unqualified import (newtype)" $ do
    withSupportFiles (Integration.addImport "MyNewtype")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (MyNewtype(MyNewtype))"
                                           , "import Main (id)"])
  it "adds an explicit unqualified import (typeclass member function)" $ do
    withSupportFiles (Integration.addImport "typeClassFun")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (typeClassFun)"
                                           , "import Main (id)"])
  it "doesn't add a newtypes constructor if only the type is exported" $ do
    withSupportFiles (Integration.addImport "OnlyTypeExported")
    outputFileShouldBe (sourceFileSkeleton [ "import ImportsSpec1 (OnlyTypeExported)"
                                            , "import Main (id)"])
  it "doesn't add an import if the identifier is defined in the module itself" $ do
    withSupportFiles (Integration.addImport "myId")
    outputFileShouldBe (sourceFileSkeleton [ "import Main (id)"])
  it "responds with an error if it's undecidable whether we want a type or constructor" $
    withSupportFiles (\sourceFp outFp -> do
                         r <- Integration.addImport "SpecialCase" sourceFp outFp
                         shouldBe False (Integration.resultIsSuccess r)
                         shouldBe False =<< doesFileExist outFp)
  it "responds with an error if the identifier cannot be found and doesn't \
     \write to the output file" $
    withSupportFiles (\sourceFp outFp -> do
                         r <- Integration.addImport "doesntExist" sourceFp outFp
                         shouldBe False (Integration.resultIsSuccess r)
                         shouldBe False =<< doesFileExist outFp)
