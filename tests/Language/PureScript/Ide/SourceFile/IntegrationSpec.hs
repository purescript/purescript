{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.SourceFile.IntegrationSpec where


import           Protolude

import qualified Data.Text                           as T
import qualified Language.PureScript.Ide.Integration as Integration
import qualified Language.PureScript                 as P
import           Test.Hspec

setup :: IO ()
setup = void (Integration.reset *> Integration.loadAll)

spec :: Spec
spec = beforeAll_ setup $ do
  describe "Sourcefile Integration" $ do
    it "finds a value declaration" $ do
      testCase "sfValue" (3, 1)
    it "finds a type declaration" $ do
      testCase "SFType" (5, 1)
    it "finds a data declaration" $ do
      testCase "SFData" (7, 1)
    it "finds a data constructor" $ do
      testCase "SFOne" (7, 1)
    it "finds a typeclass" $ do
      testCase "SFClass" (9, 1)
    it "finds a typeclass member" $ do
      testCase "sfShow" (10, 3)

testCase :: Text -> (Int, Int) -> IO ()
testCase s (x, y) = do
  (P.SourceSpan f (P.SourcePos l c) _):_ <- Integration.getInfo s
  toS f `shouldSatisfy` T.isSuffixOf "SourceFileSpec.purs"
  (l, c) `shouldBe` (x, y)
