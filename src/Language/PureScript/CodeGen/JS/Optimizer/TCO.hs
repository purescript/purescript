-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer.TCO
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements tail call elimination.
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.JS.Optimizer.TCO (tco) where

import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST

-- |
-- Eliminate tail calls
--
tco :: Options mode -> JS -> JS
tco opts | optionsNoTco opts = id
         | otherwise = tco'

tco' :: JS -> JS
tco' = everywhereOnJS convert
  where
  tcoLabel :: String
  tcoLabel = "tco"
  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg
  copyVar :: String -> String
  copyVar arg = "__copy_" ++ arg
  convert :: JS -> JS
  convert js@(JSVariableIntroduction name (Just fn@JSFunction {})) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = concat $ reverse argss
            in
              JSVariableIntroduction name (Just (replace (toLoop name allArgs body')))
        | otherwise -> js
  convert js = js
  collectAllFunctionArgs :: [[String]] -> (JS -> JS) -> JS -> ([[String]], JS, JS -> JS)
  collectAllFunctionArgs allArgs f (JSFunction ident args (JSBlock (body@(JSReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSFunction ident (map copyVar args) (JSBlock [b]))) body
  collectAllFunctionArgs allArgs f (JSFunction ident args body@(JSBlock _)) =
    (args : allArgs, body, f . JSFunction ident (map copyVar args))
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args (JSBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn (JSFunction ident (map copyVar args) (JSBlock [b])))) body
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args body@(JSBlock _))) =
    (args : allArgs, body, f . JSReturn . JSFunction ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)
  isTailCall :: String -> JS -> Bool
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
    countSelfCalls (JSApp (JSVar ident') _) | ident == ident' = 1
    countSelfCalls _ = 0
    countSelfCallsInTailPosition :: JS -> Int
    countSelfCallsInTailPosition (JSReturn ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0
    countSelfCallsUnderFunctions (JSFunction _ _ js') = everythingOnJS (+) countSelfCalls js'
    countSelfCallsUnderFunctions _ = 0
  toLoop :: String -> [String] -> JS -> JS
  toLoop ident allArgs js = JSBlock $
        map (\arg -> JSVariableIntroduction arg (Just (JSVar (copyVar arg)))) allArgs ++
        [ JSLabel tcoLabel $ JSWhile (JSBooleanLiteral True) (JSBlock [ everywhereOnJS loopify js ]) ]
    where
    loopify :: JS -> JS
    loopify (JSReturn ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        JSBlock $ zipWith (\val arg ->
                    JSVariableIntroduction (tcoVar arg) (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    JSAssignment (JSVar arg) (JSVar (tcoVar arg))) allArgs
                  ++ [ JSContinue tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[JS]] -> JS -> [[JS]]
    collectSelfCallArgs allArgumentValues (JSApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues
  isSelfCall :: String -> JS -> Bool
  isSelfCall ident (JSApp (JSVar ident') args) | ident == ident' && not (any isFunction args) = True
  isSelfCall ident (JSApp fn args) | not (any isFunction args) = isSelfCall ident fn
  isSelfCall _ _ = False
  isFunction :: JS -> Bool
  isFunction (JSFunction _ _ _) = True
  isFunction _ = False
