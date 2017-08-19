{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestDCE (main) where

import Prelude ()
import Prelude.Compat

import Data.List (concatMap)

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.DCE
import Language.PureScript.Names
import Language.PureScript.PSString

import Test.Hspec

main :: IO ()
main = hspec spec

getNames :: Bind a -> [Ident]
getNames (NonRec _ i _) = [i]
getNames (Rec l) = (\((_, i), _) -> i) `map` l

hasIdent :: Ident -> [Bind ()] -> Bool
hasIdent i = (i `elem`) . concatMap getNames

spec :: Spec
spec = context "DCE" $ do
  context "dceExpr" $ do
    specify "should remove unused identifier" $ do
      let e :: Expr ()
          e = Let ()
                [ NonRec () (Ident "notUsed") (Literal () (CharLiteral 'a'))
                , NonRec () (Ident "used") (Literal () (CharLiteral 'b'))
                ]
                (Var () (Qualified Nothing (Ident "used")))
      case dceExpr (NonRec () (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` not . hasIdent (Ident "notUsed")
          bs `shouldSatisfy` hasIdent (Ident "used")
        _ -> return ()

    specify "should not remove transitive dependency" $ do
      let e :: Expr ()
          e = Let ()
                [ NonRec () (Ident "used") (Abs () (Ident "x") (Var () (Qualified Nothing (Ident "trDep"))))
                , NonRec () (Ident "trDep") (Literal () (CharLiteral 'a'))
                ]
                (Var () (Qualified Nothing (Ident "used")))
      case dceExpr (NonRec () (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` hasIdent (Ident "trDep")
          bs `shouldSatisfy` hasIdent (Ident "used")
        _ -> return ()

    specify "should include all used recursive binds" $ do
      let e :: Expr ()
          e = Let ()
                [ NonRec () (Ident "entry") (Abs () (Ident "x") (Var () (Qualified Nothing (Ident "mutDep1"))))
                , Rec
                  [ (((), Ident "mutDep1"), Abs () (Ident "x") (Var () (Qualified Nothing (Ident "mutDep2"))))
                  , (((), Ident "mutDep2"), Abs () (Ident "x") (Var () (Qualified Nothing (Ident "mutDep1"))))
                  ]
                ]
                (App () (Var () (Qualified Nothing (Ident "entry"))) (Literal () (CharLiteral 'a')))
      case dceExpr (NonRec () (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` hasIdent (Ident "entry")
          bs `shouldSatisfy` hasIdent (Ident "mutDep1")
          bs `shouldSatisfy` hasIdent (Ident "mutDep2")
        _ -> return ()

    specify "should dce case expressions" $ do
      let e :: Expr ()
          e = Let ()
                [ NonRec () (Ident "usedInExpr") (Literal () (CharLiteral 'a'))
                , NonRec () (Ident "notUsed") (Literal () (CharLiteral 'a'))
                , NonRec () (Ident "usedInGuard") (Literal () (CharLiteral 'a'))
                , NonRec () (Ident "usedInResult1") (Literal () (CharLiteral 'a'))
                , NonRec () (Ident "usedInResult2") (Literal () (CharLiteral 'a'))
                ]
                (Case ()
                  [Var () (Qualified Nothing (Ident "usedInExpr"))]
                  [ CaseAlternative
                      [NullBinder ()]
                      (Left
                        [ ( Var () (Qualified Nothing (Ident "usedInGuard"))
                          , Var () (Qualified Nothing (Ident "usedInResult1"))
                          )
                        ])
                  , CaseAlternative
                      [NullBinder ()]
                      (Right $ Var () (Qualified Nothing (Ident "usedInResult2")))
                  ])
      case dceExpr (NonRec () (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` hasIdent (Ident "usedInExpr")
          bs `shouldSatisfy` not . hasIdent (Ident "notUsed")
          bs `shouldSatisfy` hasIdent (Ident "usedInGuard")
          bs `shouldSatisfy` hasIdent (Ident "usedInResult1")
          bs `shouldSatisfy` hasIdent (Ident "usedInResult2")
        _ -> return ()

  specify "it should not remove shadowed identifiers" $ do
    let e :: Expr ()
        e = Let ()
              [ NonRec () (Ident "shadow") (Literal () (CharLiteral 'a'))
              , NonRec () (Ident "sunny") (Literal () (CharLiteral 'a'))
              ]
              $ Let ()
                [ NonRec () (Ident "shadow") (Literal () (CharLiteral 'a')) ]
                $ Literal ()
                  $ ObjectLiteral 
                    [ ( mkString "a", Var () (Qualified Nothing (Ident "shadow")) )
                    , ( mkString "b", Var () (Qualified Nothing (Ident "sunny")) )
                    ]
    case dceExpr (NonRec () (Ident "v") e) of
      NonRec _ _ (Let _ bs (Let () cs _)) -> do
        bs `shouldSatisfy` hasIdent (Ident "sunny")
        bs `shouldSatisfy` not . hasIdent (Ident "shadow")
        cs `shouldSatisfy` hasIdent (Ident "shadow")
      _ -> undefined
