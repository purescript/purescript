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
-- This module optimizes code in the simplified-Javascript intermediate representation.
--
-- The following optimizations are supported:
--
--  * Collapsing nested blocks
--
--  * Tail call elimination
--
--  * Inlining of (>>=) and ret for the Eff monad
--
--  * Removal of unused variables
--
--  * Removal of unnecessary thunks
--
--  * Eta conversion
--
--  * Inlining variables
--
--  * Inline Prelude.($)
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Optimize (
    optimize
) where

import Data.Data
import Data.Maybe (fromMaybe)
import Data.Generics

import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options
import Language.PureScript.CodeGen.Common (identToJs)
import Language.PureScript.Sugar.TypeClasses
       (mkDictionaryValueName)
import Language.PureScript.Types (Type(..))

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: Options -> JS -> JS
optimize opts = untilFixedPoint $ applyAll
  [ collapseNestedBlocks
  , tco opts
  , magicDo opts
  , removeUnusedVariables
  , unThunk
  , etaConvert
  , inlineVariables
  , inlineDollar ]

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f a = go a
  where
  go a' = let a'' = f a' in
          if a'' == a' then a'' else go a''

replaceIdent :: (Data d) => String -> JS -> d -> d
replaceIdent var1 js = everywhere (mkT replace)
  where
  replace (JSVar var2) | var1 == var2 = js
  replace other = other

replaceIdents :: (Data d) => [(String, JS)] -> d -> d
replaceIdents vars = everywhere (mkT replace)
  where
  replace v@(JSVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: (Data d) => String -> d -> Bool
isReassigned var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSFunction _ args _) | var1 `elem` args = True
  check _ = False

isRebound :: (Data d) => JS -> d -> Bool
isRebound js d = any (\var -> isReassigned var d) (variablesOf js)
  where
  variablesOf (JSVar var) = [var]
  variablesOf (JSAccessor _ val) = variablesOf val
  variablesOf (JSIndexer index val) = variablesOf index ++ variablesOf val
  variablesOf _ = []

isUsed :: (Data d) => String -> d -> Bool
isUsed var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSVar var2) | var1 == var2 = True
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: JSAssignment -> String
targetVariable (JSAssignVariable var) = var
targetVariable (JSAssignProperty _ tgt) = targetVariable tgt

isUpdated :: (Data d) => String -> d -> Bool
isUpdated var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

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
  go (s@(JSVariableIntroduction var (Just js)) : sts)
    | shouldInline js && not (isReassigned var sts) && not (isRebound js sts) && not (isUpdated var sts) =
      go (replaceIdent var js sts)
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
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing idents block@(JSBlock body)) args)])
    | all shouldInline args && not (or (map (flip isRebound block) args)) = JSBlock (replaceIdents (zip idents args) body)
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
  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg
  copyVar :: String -> String
  copyVar arg = "__copy_" ++ arg
  convert :: JS -> JS
  convert js@(JSVariableIntroduction name (Just fn@(JSFunction _ _ _))) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = reverse $ concat argss
            in
              JSVariableIntroduction name (Just (replace (toLoop name allArgs body')))
        | otherwise -> js
  convert js = js
  collectAllFunctionArgs :: [[String]] -> (JS -> JS) -> JS -> ([[String]], JS, JS -> JS)
  collectAllFunctionArgs allArgs f (JSFunction ident args (JSBlock (body@(JSReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSFunction ident (map copyVar args) (JSBlock [b]))) body
  collectAllFunctionArgs allArgs f (JSFunction ident args body@(JSBlock _)) =
    (args : allArgs, body, \b -> f (JSFunction ident (map copyVar args) b))
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args (JSBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn (JSFunction ident (map copyVar args) (JSBlock [b])))) body
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args body@(JSBlock _))) =
    (args : allArgs, body, \b -> f (JSReturn (JSFunction ident (map copyVar args) b)))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)
  isTailCall :: String -> JS -> Bool
  isTailCall ident js =
    let
      numSelfCalls = everything (+) (mkQ 0 countSelfCalls) js
      numSelfCallsInTailPosition = everything (+) (mkQ 0 countSelfCallsInTailPosition) js
      numSelfCallsUnderFunctions = everything (+) (mkQ 0 countSelfCallsUnderFunctions) js
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
    countSelfCallsUnderFunctions (JSFunction _ _ js') = everything (+) (mkQ 0 countSelfCalls) js'
    countSelfCallsUnderFunctions _ = 0
  toLoop :: String -> [String] -> JS -> JS
  toLoop ident allArgs js = JSBlock $
        map (\arg -> JSVariableIntroduction arg (Just (JSVar (copyVar arg)))) allArgs ++
        [ JSLabel tcoLabel $ JSWhile (JSBooleanLiteral True) (JSBlock [ everywhere (mkT loopify) js ]) ]
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
  isSelfCall :: String -> JS -> Bool
  isSelfCall ident (JSApp (JSVar ident') _) | ident == ident' = True
  isSelfCall ident (JSApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = False

magicDo :: Options -> JS -> JS
magicDo opts | optionsMagicDo opts = magicDo'
             | otherwise = id

magicDo' :: JS -> JS
magicDo' = everywhere (mkT undo) . everywhere' (mkT convert)
  where
  fnName = "__do"
  convert :: JS -> JS
  convert (JSApp (JSApp ret [val]) []) | isReturn ret = val
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing ["_"] (JSBlock [JSReturn ret])]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock [ JSApp m [], JSReturn (JSApp ret []) ]
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing [arg] (JSBlock [JSReturn ret])]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock [ JSVariableIntroduction arg (Just (JSApp m [])), JSReturn (JSApp ret []) ]
  convert other = other
  isBind (JSApp bindPoly [JSApp effDict []]) | isBindPoly bindPoly && isEffDict effDict = True
  isBind _ = False
  isReturn (JSApp retPoly [JSApp effDict []]) | isRetPoly retPoly && isEffDict effDict = True
  isReturn _ = False
  isBindPoly (JSVar op) | op == identToJs (Op ">>=") = True
  isBindPoly (JSAccessor prop (JSVar "Prelude")) | prop == identToJs (Op ">>=") = True
  isBindPoly _ = False
  isRetPoly (JSVar "ret") = True
  isRetPoly (JSAccessor "ret" (JSVar "Prelude")) = True
  isRetPoly _ = False
  prelude = ModuleName (ProperName "Prelude")
  effModule = ModuleName (ProperName "Eff")
  Right (Ident effDictName) = mkDictionaryValueName
    effModule
    (Qualified (Just prelude) (ProperName "Monad"))
    (TypeConstructor (Qualified (Just effModule) (ProperName "Eff")))
  isEffDict (JSVar ident) | ident == effDictName = True
  isEffDict (JSAccessor prop (JSVar "Eff")) | prop == effDictName = True
  isEffDict _ = False
  undo :: JS -> JS
  undo (JSReturn (JSApp (JSFunction (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

collapseNestedBlocks :: JS -> JS
collapseNestedBlocks = everywhere (mkT collapse)
  where
  collapse :: JS -> JS
  collapse (JSBlock sts) = JSBlock (concatMap go sts)
  collapse js = js
  go :: JS -> [JS]
  go (JSBlock sts) = sts
  go s = [s]

inlineDollar :: JS -> JS
inlineDollar = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSApp (JSApp dollar [f]) [x]) | isDollar dollar = JSApp f [x]
  convert other = other
  isDollar (JSAccessor name (JSVar "Prelude")) | name == identToJs (Op "$") = True
  isDollar _ = False
