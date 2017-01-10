{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.SourceFile1Spec where

import Protolude
import qualified Data.Text as T

import Test.Hspec
import Language.PureScript.Ide.Test
import Language.PureScript.Ide.Command
import Language.PureScript.Ide.Types
import qualified Language.PureScript as P

spec :: Spec
spec = do
  describe "Sourcefile Integration" $ do
    it "finds a value declaration" $ do
      Just r <- getLocation "sfValue"
      r `shouldBe` valueSS
    it "finds a synonym declaration" $ do
      Just r <- getLocation "SFType"
      r `shouldBe` synonymSS
    it "finds a data declaration" $ do
      rs <- traverse getLocation ["SFData", "SFOne", "SFTwo", "SFThree"]
      traverse_ (`shouldBe` (Just typeSS)) rs
    it "finds a class declaration" $ do
      Just r <- getLocation "SFClass"
      r `shouldBe` classSS

getLocation :: Text -> IO (Maybe P.SourceSpan)
getLocation s = do
  ([Right (CompletionResult [c])], _) <- runIde' defConfig ideState [Type s [] Nothing]
  pure (complLocation c)
  where
    ideState = emptyIdeState `s3` [ ("Test", [ ideValue "sfValue" Nothing `annLoc` valueSS
                                             , ideSynonym "SFType" P.tyString `annLoc` synonymSS
                                             , ideType "SFData" Nothing `annLoc` typeSS
                                             , ideDtor "SFOne" "SFData" Nothing `annLoc` typeSS
                                             , ideDtor "SFTwo" "SFData" Nothing `annLoc` typeSS
                                             , ideDtor "SFThree" "SFData" Nothing `annLoc` typeSS
                                             , ideTypeClass "SFClass" `annLoc` classSS
                                             ])
                                  ]

ss :: Int -> Int -> P.SourceSpan
ss x y = P.SourceSpan "Test.purs" (P.SourcePos x y) (P.SourcePos x y)

valueSS :: P.SourceSpan
valueSS = ss 3 1

synonymSS :: P.SourceSpan
synonymSS = ss 5 1

typeSS :: P.SourceSpan
typeSS = ss 7 1

classSS :: P.SourceSpan
classSS = ss 8 1

sf :: Text
sf = T.unlines
  [ "module SourceFileSpec where"
  , ""
  , "sfValue = \"sfValue\""
  , ""
  , "type SFType = String"
  , ""
  , "data SFData = SFOne | SFTwo | SFThree"
  , ""
  , "class SFClass a where"
  , " sfShow :: a -> String"
  ]
