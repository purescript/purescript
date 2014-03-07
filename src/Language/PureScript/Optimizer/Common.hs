-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer.Common
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

module Language.PureScript.Optimizer.Common where

import Data.Maybe (fromMaybe)
import Data.Generics

import Language.PureScript.CodeGen.JS.AST

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

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
  check (JSVariableIntroduction arg _) | var1 == arg = True
  check (JSAssignment (JSVar arg) _) | var1 == arg = True
  check _ = False

isRebound :: (Data d) => JS -> d -> Bool
isRebound js d = any (`isReassigned` d) (everything (++) (mkQ [] variablesOf) js)
  where
  variablesOf (JSVar var) = [var]
  variablesOf _ = []

isUsed :: (Data d) => String -> d -> Bool
isUsed var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSVar var2) | var1 == var2 = True
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: JS -> String
targetVariable (JSVar var) = var
targetVariable (JSAccessor _ tgt) = targetVariable tgt
targetVariable (JSIndexer _ tgt) = targetVariable tgt
targetVariable _ = error "Invalid argument to targetVariable"

isUpdated :: (Data d) => String -> d -> Bool
isUpdated var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([JS] -> [JS]) -> JS -> JS
removeFromBlock go (JSBlock sts) = JSBlock (go sts)
removeFromBlock _  js = js
