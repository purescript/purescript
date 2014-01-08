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
optimize opts = tco opts . removeUnusedVariables . unThunk . etaConvert . inlineVariables

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
  convert :: JS -> JS
  convert (JSVariableIntroduction name (Just (JSFunction Nothing args body)))
    | isTailCall name body = (JSVariableIntroduction name (Just (JSFunction Nothing args (toLoop name args body))))
  convert js = js
  isTailCall :: Ident -> JS -> Bool
  isTailCall _ _ = False
  toLoop :: Ident -> [Ident] -> JS -> JS
  toLoop = undefined
