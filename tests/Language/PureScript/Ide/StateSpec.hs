{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.PureScript.Ide.StateSpec where

import           Protolude
import           Control.Lens hiding ((&))
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Test
import qualified Language.PureScript as P
import           Test.Hspec
import qualified Data.Map as Map

valueOperator :: Maybe P.Type -> IdeDeclarationAnn
valueOperator =
  ideValueOp "<$>" (P.Qualified (Just (mn "Test")) (Left "function")) 2 Nothing

ctorOperator :: Maybe P.Type -> IdeDeclarationAnn
ctorOperator =
  ideValueOp ":" (P.Qualified (Just (mn "Test")) (Right "Cons")) 2 Nothing

typeOperator :: Maybe P.Kind -> IdeDeclarationAnn
typeOperator =
  ideTypeOp ":" (P.Qualified (Just (mn "Test")) "List") 2 Nothing

testModule :: (P.ModuleName, [IdeDeclarationAnn])
testModule =
  (mn "Test",
    [ ideValue "function" (Just P.REmpty)
    , ideDtor "Cons" "List" (Just P.tyString)
    , ideType "List" Nothing []
    , valueOperator Nothing
    , ctorOperator Nothing
    , typeOperator Nothing
    ])

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
      -- , edInstanceChain =
      mempty
      -- , edInstanceChainIndex =
      0
 --     }
    ]
  --, efSourceSpan =
    (P.internalModuleSourceSpan "<tests>")
 -- }

moduleMap :: ModuleMap [IdeDeclarationAnn]
moduleMap = Map.singleton (mn "ClassModule") [ideTypeClass "MyClass" P.kindType []]

ideInstance :: IdeInstance
ideInstance = IdeInstance (mn "InstanceModule") (P.Ident "myClassInstance") mempty mempty

spec :: Spec
spec = do
  describe "resolving operators" $ do
    it "resolves the type for a value operator" $
      resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (valueOperator (Just P.REmpty))
    it "resolves the type for a constructor operator" $
      resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (ctorOperator (Just P.tyString))
    it "resolves the kind for a type operator" $
      resolveOperatorsForModule testState (snd testModule) `shouldSatisfy` elem (typeOperator (Just P.kindType))
  describe "resolving instances for type classes" $ do
    it "resolves an instance for an existing type class" $ do
      resolveInstances (Map.singleton (mn "InstanceModule") ef) moduleMap
        `shouldSatisfy`
        elemOf (ix (mn "ClassModule") . ix 0 . idaDeclaration . _IdeDeclTypeClass . ideTCInstances . folded) ideInstance
  describe "resolving data constructors" $ do
    it "resolves a constructor" $ do
      resolveDataConstructorsForModule (snd testModule)
        `shouldSatisfy`
        elem (ideType "List" Nothing [(P.ProperName "Cons", P.tyString)])
