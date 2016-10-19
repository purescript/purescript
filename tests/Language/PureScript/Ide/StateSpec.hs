{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.StateSpec where

import           Protolude
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.State
import qualified Language.PureScript as P
import           Test.Hspec
import qualified Data.Map as Map

valueOperator :: Maybe P.Type -> IdeDeclarationAnn
valueOperator =
  d . IdeDeclValueOperator . IdeValueOperator (P.OpName "<$>") (P.Qualified (Just (mn "Test")) (Left (P.Ident "function"))) 2 P.Infix

ctorOperator :: Maybe P.Type -> IdeDeclarationAnn
ctorOperator =
  d . IdeDeclValueOperator . IdeValueOperator (P.OpName ":") (P.Qualified (Just (mn "Test")) (Right (P.ProperName "Cons"))) 2 P.Infix

typeOperator :: Maybe P.Kind -> IdeDeclarationAnn
typeOperator =
  d . IdeDeclTypeOperator . IdeTypeOperator (P.OpName ":") (P.Qualified (Just (mn "Test")) (P.ProperName "List")) 2 P.Infix

testModule :: Module
testModule = (mn "Test", [ d (IdeDeclValue (IdeValue (P.Ident "function") P.REmpty))
                         , d (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName "Cons") (P.ProperName "List") (P.REmpty)))
                         , d (IdeDeclType (IdeType (P.ProperName "List") P.Star))
                         , valueOperator Nothing
                         , ctorOperator Nothing
                         , typeOperator Nothing
                         ])

d :: IdeDeclaration -> IdeDeclarationAnn
d = IdeDeclarationAnn emptyAnn

mn :: Text -> P.ModuleName
mn = P.moduleNameFromString . toS

testState :: Map P.ModuleName [IdeDeclarationAnn]
testState = Map.fromList
  [ testModule
  ]

spec :: Spec
spec = describe "resolving operators" $ do
  it "resolves the type for a value operator" $
    resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (valueOperator (Just P.REmpty))
  it "resolves the type for a constructor operator" $
    resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (ctorOperator (Just P.REmpty))
  it "resolves the kind for a type operator" $
    resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (typeOperator (Just P.Star))
