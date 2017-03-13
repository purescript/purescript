-- | This module implements tail call elimination.
module Language.PureScript.CoreImp.Optimizer.TCO (tco) where

import Prelude.Compat

import Data.Text (Text)
import Data.Monoid ((<>))
import Language.PureScript.CoreImp.AST
import Language.PureScript.AST.SourcePos (SourceSpan)

-- | Eliminate tail calls
tco :: AST -> AST
tco = everywhere convert where
  tcoVar :: Text -> Text
  tcoVar arg = "__tco_" <> arg

  copyVar :: Text -> Text
  copyVar arg = "__copy_" <> arg

  tcoDone :: Text
  tcoDone = tcoVar "done"

  tcoLoop :: Text
  tcoLoop = tcoVar "loop"

  tcoResult :: Text
  tcoResult = tcoVar "result"

  convert :: AST -> AST
  convert (VariableIntroduction ss name (Just fn@Function {}))
      | isTailRecursive name body'
      = VariableIntroduction ss name (Just (replace (toLoop name allArgs body')))
    where
      (argss, body', replace) = collectAllFunctionArgs [] id fn
      allArgs = concat $ reverse argss
  convert js = js

  collectAllFunctionArgs :: [[Text]] -> (AST -> AST) -> AST -> ([[Text]], AST, AST -> AST)
  collectAllFunctionArgs allArgs f (Function s1 ident args (Block s2 (body@(Return _ _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (Function s1 ident (map copyVar args) (Block s2 [b]))) body
  collectAllFunctionArgs allArgs f (Function ss ident args body@(Block _ _)) =
    (args : allArgs, body, f . Function ss ident (map copyVar args))
  collectAllFunctionArgs allArgs f (Return s1 (Function s2 ident args (Block s3 [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (Return s1 (Function s2 ident (map copyVar args) (Block s3 [b])))) body
  collectAllFunctionArgs allArgs f (Return s1 (Function s2 ident args body@(Block _ _))) =
    (args : allArgs, body, f . Return s1 . Function s2 ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  isTailRecursive :: Text -> AST -> Bool
  isTailRecursive ident js = countSelfReferences js > 0 && allInTailPosition js where
    countSelfReferences = everything (+) match where
      match :: AST -> Int
      match (Var _ ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition (Return _ expr)
      | isSelfCall ident expr = countSelfReferences expr == 1
      | otherwise = countSelfReferences expr == 0
    allInTailPosition (While _ js1 body)
      = countSelfReferences js1 == 0 && allInTailPosition body
    allInTailPosition (For _ _ js1 js2 body)
      = countSelfReferences js1 == 0 && countSelfReferences js2 == 0 && allInTailPosition body
    allInTailPosition (ForIn _ _ js1 body)
      = countSelfReferences js1 == 0 && allInTailPosition body
    allInTailPosition (IfElse _ js1 body el)
      = countSelfReferences js1 == 0 && allInTailPosition body && all allInTailPosition el
    allInTailPosition (Block _ body)
      = all allInTailPosition body
    allInTailPosition (Throw _ js1)
      = countSelfReferences js1 == 0
    allInTailPosition _
      = False

  toLoop :: Text -> [Text] -> AST -> AST
  toLoop ident allArgs js =
      Block rootSS $
        map (\arg -> VariableIntroduction rootSS arg (Just (Var rootSS (copyVar arg)))) allArgs ++
        [ VariableIntroduction rootSS tcoDone (Just (BooleanLiteral rootSS False))
        , VariableIntroduction rootSS tcoResult Nothing
        ] ++
        map (\arg ->
          VariableIntroduction rootSS (tcoVar arg) Nothing) allArgs ++
        [ Function rootSS (Just tcoLoop) allArgs (Block rootSS [loopify js])
        , While rootSS (Unary rootSS Not (Var rootSS tcoDone))
            (Block rootSS
              (Assignment rootSS (Var rootSS tcoResult) (App rootSS (Var rootSS tcoLoop) (map (Var rootSS) allArgs))
              : map (\arg ->
                  Assignment rootSS (Var rootSS arg) (Var rootSS (tcoVar arg))) allArgs))
        , Return rootSS (Var rootSS tcoResult)
        ]
    where
    rootSS = Nothing

    loopify :: AST -> AST
    loopify (Return ss ret)
      | isSelfCall ident ret =
        let
          allArgumentValues = concat $ collectArgs [] ret
        in
          Block ss $
            zipWith (\val arg ->
              Assignment ss (Var ss (tcoVar arg)) val) allArgumentValues allArgs
            ++ [ ReturnNoResult ss ]
      | otherwise = Block ss [ markDone ss, Return ss ret ]
    loopify (ReturnNoResult ss) = Block ss [ markDone ss, ReturnNoResult ss ]
    loopify (While ss cond body) = While ss cond (loopify body)
    loopify (For ss i js1 js2 body) = For ss i js1 js2 (loopify body)
    loopify (ForIn ss i js1 body) = ForIn ss i js1 (loopify body)
    loopify (IfElse ss cond body el) = IfElse ss cond (loopify body) (fmap loopify el)
    loopify (Block ss body) = Block ss (map loopify body)
    loopify other = other

    markDone :: Maybe SourceSpan -> AST
    markDone ss = Assignment ss (Var ss tcoDone) (BooleanLiteral ss True)

    collectArgs :: [[AST]] -> AST -> [[AST]]
    collectArgs acc (App _ fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

  isSelfCall :: Text -> AST -> Bool
  isSelfCall ident (App _ (Var _ ident') _) = ident == ident'
  isSelfCall ident (App _ fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
