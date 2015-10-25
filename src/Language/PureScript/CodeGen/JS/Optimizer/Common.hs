-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer.Common
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common functions used by the various optimizer phases
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.JS.Optimizer.Common where

import Data.Maybe (fromMaybe)

import Language.PureScript.Crash
import Language.PureScript.CodeGen.JS.AST

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

replaceIdent :: String -> JS -> JS -> JS
replaceIdent var1 js = everywhereOnJS replace
  where
  replace (JSVar var2) | var1 == var2 = js
  replace other = other

replaceIdents :: [(String, JS)] -> JS -> JS
replaceIdents vars = everywhereOnJS replace
  where
  replace v@(JSVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: String -> JS -> Bool
isReassigned var1 = everythingOnJS (||) check
  where
  check :: JS -> Bool
  check (JSFunction _ args _) | var1 `elem` args = True
  check (JSVariableIntroduction arg _) | var1 == arg = True
  check (JSAssignment (JSVar arg) _) | var1 == arg = True
  check (JSFor arg _ _ _) | var1 == arg = True
  check (JSForIn arg _ _) | var1 == arg = True
  check _ = False

isRebound :: JS -> JS -> Bool
isRebound js d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnJS (++) variablesOf js)
  where
  variablesOf (JSVar var) = [var]
  variablesOf _ = []

isUsed :: String -> JS -> Bool
isUsed var1 = everythingOnJS (||) check
  where
  check :: JS -> Bool
  check (JSVar var2) | var1 == var2 = True
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: JS -> String
targetVariable (JSVar var) = var
targetVariable (JSAccessor _ tgt) = targetVariable tgt
targetVariable (JSIndexer _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: String -> JS -> Bool
isUpdated var1 = everythingOnJS (||) check
  where
  check :: JS -> Bool
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([JS] -> [JS]) -> JS -> JS
removeFromBlock go (JSBlock sts) = JSBlock (go sts)
removeFromBlock _  js = js
