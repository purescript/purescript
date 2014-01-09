-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimize
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Optimize (
    optimize
) where

import Data.Data
import Data.Maybe (fromMaybe)
import Data.Generics

import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options

optimize :: Options -> JS -> JS
optimize opts = removeUnusedVariables . unThunk . etaConvert . inlineVariables . tco opts

replaceIdent :: (Data d) => Ident -> JS -> d -> d
replaceIdent var1 js = everywhere (mkT replace)
  where
  replace (JSVar var2) | var1 == var2 = js
  replace other = other

replaceIdents :: (Data d) => [(Ident, JS)] -> d -> d
replaceIdents vars = everywhere (mkT replace)
  where
  replace v@(JSVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: (Data d) => Ident -> d -> Bool
isReassigned var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSAssignment (JSAssignVariable var2) _) | var1 == var2 = True
  check _ = False

isUsed :: (Data d) => Ident -> d -> Bool
isUsed var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSVar var2) | var1 == var2 = True
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False
  targetVariable :: JSAssignment -> Ident
  targetVariable (JSAssignVariable var) = var
  targetVariable (JSAssignProperty _ tgt) = targetVariable tgt

shouldInline :: JS -> Bool
shouldInline (JSVar _) = True
shouldInline (JSNumericLiteral _) = True
shouldInline (JSStringLiteral _) = True
shouldInline (JSBooleanLiteral _) = True
shouldInline (JSAccessor _ val) = shouldInline val
shouldInline (JSIndexer index val) = shouldInline index && shouldInline val
shouldInline _ = False

inlineVariables :: JS -> JS
inlineVariables = everywhere (mkT removeFromBlock)
  where
  removeFromBlock :: JS -> JS
  removeFromBlock (JSBlock sts) = JSBlock (go sts)
  removeFromBlock js = js
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var (Just js) : sts) | shouldInline js && not (isReassigned var sts) = go (replaceIdent var js sts)
  go (s:sts) = s : go sts

removeUnusedVariables :: JS -> JS
removeUnusedVariables = everywhere (mkT removeFromBlock)
  where
  removeFromBlock :: JS -> JS
  removeFromBlock (JSBlock sts) = JSBlock (go sts)
  removeFromBlock js = js
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var _ : sts) | not (isUsed var sts) = go sts
  go (s:sts) = s : go sts

etaConvert :: JS -> JS
etaConvert = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing idents (JSBlock body)) args)])
    | all shouldInline args = JSBlock (replaceIdents (zip idents args) body)
  convert js = js

unThunk :: JS -> JS
unThunk = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing [] (JSBlock body)) [])]) = JSBlock body
  convert js = js

tco :: Options -> JS -> JS
tco opts | optionsTco opts = tco'
         | otherwise = id

tco' :: JS -> JS
tco' = everywhere (mkT convert)
  where
  tcoLabel :: String
  tcoLabel = "tco"
  tcoRet :: Ident
  tcoRet = Ident "__tco_ret"
  tcoVar :: Ident -> Ident
  tcoVar (Ident arg) = Ident $ "__tco_" ++ arg
  tcoVar _ = error "Invalid name in tcoVar"
  convert :: JS -> JS
  convert (JSVariableIntroduction name (Just fn@(JSFunction Nothing _ body))) | isTailCall name body =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
      allArgs = reverse $ concat argss
    in
      JSVariableIntroduction name (Just (replace (toLoop name allArgs body')))
  convert js = js
  collectAllFunctionArgs :: [[Ident]] -> (JS -> JS) -> JS -> ([[Ident]], JS, JS -> JS)
  collectAllFunctionArgs allArgs f (JSFunction Nothing args (JSBlock [body])) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSFunction Nothing args (JSBlock [b]))) body
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction Nothing args (JSBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn (JSFunction Nothing args (JSBlock [b])))) body
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)
  isTailCall :: Ident -> JS -> Bool
  isTailCall ident js =
    let
      numSelfCalls = everything (+) (mkQ 0 countSelfCalls) js
      numSelfCallsInTailPosition = everything (+) (mkQ 0 countSelfCallsInTailPosition) js
    in
      numSelfCalls > 0 && numSelfCalls == numSelfCallsInTailPosition
    where
    countSelfCalls :: JS -> Int
    countSelfCalls (JSApp (JSVar ident') _) | ident == ident' = 1
    countSelfCalls _ = 0
    countSelfCallsInTailPosition :: JS -> Int
    countSelfCallsInTailPosition (JSReturn ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0
  toLoop :: Ident -> [Ident] -> JS -> JS
  toLoop ident allArgs js = JSBlock
        [ JSVariableIntroduction tcoRet Nothing
        , JSLabel tcoLabel $ JSWhile (JSBooleanLiteral True) (JSBlock [ everywhere (mkT loopify) js ]) ]
    where
    loopify :: JS -> JS
    loopify (JSReturn ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        JSBlock $ zipWith (\val arg ->
                    JSVariableIntroduction (tcoVar arg) (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    JSAssignment (JSAssignVariable arg) (JSVar (tcoVar arg))) allArgs
                  ++ [ JSContinue tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[JS]] -> JS -> [[JS]]
    collectSelfCallArgs allArgumentValues (JSApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues
  isSelfCall :: Ident -> JS -> Bool
  isSelfCall ident (JSApp (JSVar ident') _) | ident == ident' = True
  isSelfCall ident (JSApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
