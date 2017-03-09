-- | This module implements tail call elimination.
module Language.PureScript.CodeGen.JS.Optimizer.TCO (tco) where

import Prelude.Compat

import Data.Text (Text)
import Data.Monoid ((<>))
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.AST.SourcePos (SourceSpan)

-- | Eliminate tail calls
tco :: JS -> JS
tco = everywhereOnJS convert where
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

  convert :: JS -> JS
  convert (JSVariableIntroduction ss name (Just fn@JSFunction {}))
      | isTailRecursive name body'
      = JSVariableIntroduction ss name (Just (replace (toLoop name allArgs body')))
    where
      (argss, body', replace) = collectAllFunctionArgs [] id fn
      allArgs = concat $ reverse argss
  convert js = js

  collectAllFunctionArgs :: [[Text]] -> (JS -> JS) -> JS -> ([[Text]], JS, JS -> JS)
  collectAllFunctionArgs allArgs f (JSFunction s1 ident args (JSBlock s2 (body@(JSReturn _ _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSFunction s1 ident (map copyVar args) (JSBlock s2 [b]))) body
  collectAllFunctionArgs allArgs f (JSFunction ss ident args body@(JSBlock _ _)) =
    (args : allArgs, body, f . JSFunction ss ident (map copyVar args))
  collectAllFunctionArgs allArgs f (JSReturn s1 (JSFunction s2 ident args (JSBlock s3 [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn s1 (JSFunction s2 ident (map copyVar args) (JSBlock s3 [b])))) body
  collectAllFunctionArgs allArgs f (JSReturn s1 (JSFunction s2 ident args body@(JSBlock _ _))) =
    (args : allArgs, body, f . JSReturn s1 . JSFunction s2 ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  isTailRecursive :: Text -> JS -> Bool
  isTailRecursive ident js = countSelfReferences js > 0 && allInTailPosition js where
    countSelfReferences = everythingOnJS (+) match where
      match :: JS -> Int
      match (JSVar _ ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition (JSReturn _ expr)
      | isSelfCall ident expr = countSelfReferences expr == 1
      | otherwise = countSelfReferences expr == 0
    allInTailPosition (JSWhile _ js1 body)
      = countSelfReferences js1 == 0 && allInTailPosition body
    allInTailPosition (JSFor _ _ js1 js2 body)
      = countSelfReferences js1 == 0 && countSelfReferences js2 == 0 && allInTailPosition body
    allInTailPosition (JSForIn _ _ js1 body)
      = countSelfReferences js1 == 0 && allInTailPosition body
    allInTailPosition (JSIfElse _ js1 body el)
      = countSelfReferences js1 == 0 && allInTailPosition body && all allInTailPosition el
    allInTailPosition (JSBlock _ body)
      = all allInTailPosition body
    allInTailPosition _
      = False

  toLoop :: Text -> [Text] -> JS -> JS
  toLoop ident allArgs js =
      JSBlock rootSS $
        map (\arg -> JSVariableIntroduction rootSS arg (Just (JSVar rootSS (copyVar arg)))) allArgs ++
        [ JSVariableIntroduction rootSS tcoDone (Just (JSBooleanLiteral rootSS False))
        , JSVariableIntroduction rootSS tcoResult Nothing
        ] ++
        map (\arg ->
          JSVariableIntroduction rootSS (tcoVar arg) Nothing) allArgs ++
        [ JSFunction rootSS (Just tcoLoop) allArgs (JSBlock rootSS [loopify js])
        , JSWhile rootSS (JSUnary rootSS Not (JSVar rootSS tcoDone))
            (JSBlock rootSS
              (JSAssignment rootSS (JSVar rootSS tcoResult) (JSApp rootSS (JSVar rootSS tcoLoop) (map (JSVar rootSS) allArgs))
              : map (\arg ->
                  JSAssignment rootSS (JSVar rootSS arg) (JSVar rootSS (tcoVar arg))) allArgs))
        , JSReturn rootSS (JSVar rootSS tcoResult)
        ]
    where
    rootSS = Nothing

    loopify :: JS -> JS
    loopify (JSReturn ss ret)
      | isSelfCall ident ret =
        let
          allArgumentValues = concat $ collectArgs [] ret
        in
          JSBlock ss $
            zipWith (\val arg ->
              JSAssignment ss (JSVar ss (tcoVar arg)) val) allArgumentValues allArgs
            ++ [ JSReturnNoResult ss ]
      | otherwise = JSBlock ss [ markDone ss, JSReturn ss ret ]
    loopify (JSReturnNoResult ss) = JSBlock ss [ markDone ss, JSReturnNoResult ss ]
    loopify (JSWhile ss cond body) = JSWhile ss cond (loopify body)
    loopify (JSFor ss i js1 js2 body) = JSFor ss i js1 js2 (loopify body)
    loopify (JSForIn ss i js1 body) = JSForIn ss i js1 (loopify body)
    loopify (JSIfElse ss cond body el) = JSIfElse ss cond (loopify body) (fmap loopify el)
    loopify (JSBlock ss body) = JSBlock ss (map loopify body)
    loopify other = other

    markDone :: Maybe SourceSpan -> JS
    markDone ss = JSAssignment ss (JSVar ss tcoDone) (JSBooleanLiteral ss True)

    collectArgs :: [[JS]] -> JS -> [[JS]]
    collectArgs acc (JSApp _ fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

  isSelfCall :: Text -> JS -> Bool
  isSelfCall ident (JSApp _ (JSVar _ ident') _) = ident == ident'
  isSelfCall ident (JSApp _ fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
