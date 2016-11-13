{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.Imports.IntegrationSpec where


import           Protolude

import qualified Data.Text                           as T
import qualified Language.PureScript.Ide.Integration as Integration
import           Test.Hspec

import           System.Directory
import           System.FilePath
import           System.IO.UTF8                      (readUTF8FileT)

setup :: IO ()
setup = void (Integration.reset *> Integration.loadAll)

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
  outRes <- readUTF8FileT outFp
  shouldBe (T.lines outRes) expectation

spec :: Spec
spec = beforeAll_ setup . describe "Adding imports" $ do
  let
    sourceFileSkeleton :: [Text] -> [Text]
    sourceFileSkeleton importSection =
      [ "module ImportsSpec where" , ""] ++ importSection ++ [ "" , "myId x = x"]
  it "adds an implicit import" $ do
    withSupportFiles (Integration.addImplicitImport "ImportsSpec1")
    outputFileShouldBe (sourceFileSkeleton
                        [ "import ImportsSpec1"
                        ])
  it "adds an explicit unqualified import" $ do
    withSupportFiles (Integration.addImport "exportedFunction")
    outputFileShouldBe (sourceFileSkeleton
                        [ "import ImportsSpec1 (exportedFunction)"
                        ])
  it "adds an explicit unqualified import (type)" $ do
    withSupportFiles (Integration.addImport "MyType")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (MyType)"])
  it "adds an explicit unqualified import (parameterized type)" $ do
    withSupportFiles (Integration.addImport "MyParamType")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (MyParamType)"])
  it "adds an explicit unqualified import (typeclass)" $ do
    withSupportFiles (Integration.addImport "ATypeClass")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (class ATypeClass)"])
  it "adds an explicit unqualified import (dataconstructor)" $ do
    withSupportFiles (Integration.addImport "MyJust")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (MyMaybe(..))"])
  it "adds an explicit unqualified import (newtype)" $ do
    withSupportFiles (Integration.addImport "MyNewtype")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (MyNewtype(..))"])
  it "adds an explicit unqualified import (typeclass member function)" $ do
    withSupportFiles (Integration.addImport "typeClassFun")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (typeClassFun)"])
  it "doesn't add a newtypes constructor if only the type is exported" $ do
    withSupportFiles (Integration.addImport "OnlyTypeExported")
    outputFileShouldBe (sourceFileSkeleton ["import ImportsSpec1 (OnlyTypeExported)"])
  it "doesn't add an import if the identifier is defined in the module itself" $ do
    withSupportFiles (Integration.addImport "myId")
    outputFileShouldBe (sourceFileSkeleton [])
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
