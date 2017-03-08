-- | This module implements tail call elimination.
module Language.PureScript.CodeGen.JS.Optimizer.TCO (tco) where

import Prelude.Compat

import Data.Text (Text)
import Data.Monoid ((<>))
import Language.PureScript.CodeGen.JS.AST

-- | Eliminate tail calls
tco :: JS -> JS
tco = everywhereOnJS convert where
  tcoLabel :: Text
  tcoLabel = "tco"

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
  convert js@(JSVariableIntroduction ss name (Just fn@JSFunction {})) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = concat $ reverse argss
            in
              JSVariableIntroduction ss name (Just (replace (toLoop name allArgs body')))
        | otherwise -> js
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

  isTailCall :: Text -> JS -> Bool
  isTailCall ident js =
    let
      numSelfCalls = everythingOnJS (+) countSelfCalls js
      numSelfCallsInTailPosition = everythingOnJS (+) countSelfCallsInTailPosition js
      numSelfCallsUnderFunctions = everythingOnJS (+) countSelfCallsUnderFunctions js
    in
      numSelfCalls > 0
      && numSelfCalls == numSelfCallsInTailPosition
      && numSelfCallsUnderFunctions == 0
    where
    countSelfCalls :: JS -> Int
    countSelfCalls (JSApp _ (JSVar _ ident') _) | ident == ident' = 1
    countSelfCalls _ = 0

    countSelfCallsInTailPosition :: JS -> Int
    countSelfCallsInTailPosition (JSReturn _ ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0

    countSelfCallsUnderFunctions :: JS -> Int
    countSelfCallsUnderFunctions (JSFunction _ _ _ js') = everythingOnJS (+) countSelfCalls js'
    countSelfCallsUnderFunctions _ = 0

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
        , JSLabel rootSS tcoLabel $
            JSWhile rootSS (JSUnary rootSS Not (JSVar rootSS tcoDone))
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
              ++ [ JSReturn ss (JSVar ss "undefined") ]
      | otherwise = JSBlock ss [ JSAssignment ss (JSVar ss tcoDone) (JSBooleanLiteral ss True)
                               , JSReturn ss ret
                               ]
    loopify (JSWhile ss cond body) = JSWhile ss cond (loopify body)
    loopify (JSFor ss i js1 js2 body) = JSFor ss i js1 js2 (loopify body)
    loopify (JSForIn ss i js1 body) = JSForIn ss i js1 (loopify body)
    loopify (JSIfElse ss cond body el) = JSIfElse ss cond (loopify body) (fmap loopify el)
    loopify (JSBlock ss body) = JSBlock ss (map loopify body)
    loopify (JSLabel ss l body) = JSLabel ss l (loopify body)
    loopify other = other

    collectArgs :: [[JS]] -> JS -> [[JS]]
    collectArgs acc (JSApp _ fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

  isSelfCall :: Text -> JS -> Bool
  isSelfCall ident (JSApp _ (JSVar _ ident') _) = ident == ident'
  isSelfCall ident (JSApp _ fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
