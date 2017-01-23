{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.StateSpec where

import           Protolude
import           Control.Lens hiding ((&))
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

testModule :: (P.ModuleName, [IdeDeclarationAnn])
testModule = (mn "Test", [ d (IdeDeclValue (IdeValue (P.Ident "function") P.REmpty))
                         , d (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName "Cons") (P.ProperName "List") (P.REmpty)))
                         , d (IdeDeclType (IdeType (P.ProperName "List") P.kindType))
                         , valueOperator Nothing
                         , ctorOperator Nothing
                         , typeOperator Nothing
                         ])

d :: IdeDeclaration -> IdeDeclarationAnn
d = IdeDeclarationAnn emptyAnn

mn :: Text -> P.ModuleName
mn = P.moduleNameFromString

testState :: ModuleMap [IdeDeclarationAnn]
testState = Map.fromList [testModule]

-- The accessor fields for these data types are not exposed unfortunately
ef :: P.ExternsFile
ef = P.ExternsFile
  -- { efVersion =
    mempty
  -- , efModuleName =
    (mn "InstanceModule")
  -- , efExports =
    mempty
  -- , efImports =
    mempty
  -- , efFixities =
    mempty
  -- , efTypeFixities =
    mempty
  --, efDeclarations =
    [ P.EDInstance
      -- { edInstanceClassName =
      (P.Qualified (Just (mn "ClassModule")) (P.ProperName "MyClass"))
      -- , edInstanceName =
      (P.Ident "myClassInstance")
      -- , edInstanceTypes =
      mempty
      -- , edInstanceConstraints =
      mempty
 --     }
    ]
 -- }

moduleMap :: ModuleMap [IdeDeclarationAnn]
moduleMap = Map.singleton (mn "ClassModule") [d (IdeDeclTypeClass (IdeTypeClass (P.ProperName "MyClass") []))]

ideInstance :: IdeInstance
ideInstance = IdeInstance (mn "InstanceModule") (P.Ident "myClassInstance") mempty mempty

spec :: Spec
spec = do
  describe "resolving operators" $ do
    it "resolves the type for a value operator" $
      resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (valueOperator (Just P.REmpty))
    it "resolves the type for a constructor operator" $
      resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (ctorOperator (Just P.REmpty))
    it "resolves the kind for a type operator" $
      resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (typeOperator (Just P.kindType))
  describe "resolving instances for type classes" $ do
    it "resolves an instance for an existing type class" $ do
      resolveInstances (Map.singleton (mn "InstanceModule") ef) moduleMap
        `shouldSatisfy`
        elemOf (ix (mn "ClassModule") . ix 0 . idaDeclaration . _IdeDeclTypeClass . ideTCInstances . folded) ideInstance
