-- |
-- This module implements tail call elimination.
--
module Language.PureScript.CodeGen.JS.Optimizer.TCO (tco) where

import Prelude.Compat

import Data.Text (Text)
import Data.Monoid ((<>), getAny, Any(..))

import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST

-- |
-- Eliminate tail calls
--
tco :: Options -> JS -> JS
tco opts | optionsNoTco opts = id
         | otherwise = tco'

tco' :: JS -> JS
tco' = everywhereOnJS convert
  where
  tcoLabel :: Text
  tcoLabel = "tco"

  tcoVar :: Text -> Text
  tcoVar arg = "__tco_" <> arg

  copyVar :: Text -> Text
  copyVar arg = "__copy_" <> arg

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
      numSelfCallWithFnArgs = everythingOnJS (+) countSelfCallsWithFnArgs js
    in
      numSelfCalls > 0
      && numSelfCalls == numSelfCallsInTailPosition
      && numSelfCallsUnderFunctions == 0
      && numSelfCallWithFnArgs == 0
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

    countSelfCallsWithFnArgs :: JS -> Int
    countSelfCallsWithFnArgs ret = if isSelfCallWithFnArgs ident ret [] then 1 else 0

  toLoop :: Text -> [Text] -> JS -> JS
  toLoop ident allArgs js = JSBlock rootSS $
        map (\arg -> JSVariableIntroduction rootSS arg (Just (JSVar rootSS (copyVar arg)))) allArgs ++
        [ JSLabel rootSS tcoLabel $ JSWhile rootSS (JSBooleanLiteral rootSS True) (JSBlock rootSS [ everywhereOnJS loopify js ]) ]
    where
    rootSS = Nothing

    loopify :: JS -> JS
    loopify (JSReturn ss ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        JSBlock ss $ zipWith (\val arg ->
                    JSVariableIntroduction ss (tcoVar arg) (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    JSAssignment ss (JSVar ss arg) (JSVar ss (tcoVar arg))) allArgs
                  ++ [ JSContinue ss tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[JS]] -> JS -> [[JS]]
    collectSelfCallArgs allArgumentValues (JSApp _ fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues

  isSelfCall :: Text -> JS -> Bool
  isSelfCall ident (JSApp _ (JSVar _ ident') _) = ident == ident'
  isSelfCall ident (JSApp _ fn _) = isSelfCall ident fn
  isSelfCall _ _ = False

  isSelfCallWithFnArgs :: Text -> JS -> [JS] -> Bool
  isSelfCallWithFnArgs ident (JSVar _ ident') args | ident == ident' && any hasFunction args = True
  isSelfCallWithFnArgs ident (JSApp _ fn args) acc = isSelfCallWithFnArgs ident fn (args ++ acc)
  isSelfCallWithFnArgs _ _ _ = False

  hasFunction :: JS -> Bool
  hasFunction = getAny . everythingOnJS mappend (Any . isFunction)
    where
    isFunction JSFunction{} = True
    isFunction _ = False
