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
import Data.Generics

import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST

optimize :: JS -> JS
optimize = etaConvert . inlineVariables

replaceIdent :: (Data d) => Ident -> JS -> d -> d
replaceIdent var1 js = everywhere (mkT replace)
  where
  replace (JSVar var2) | var1 == var2 = js
  replace js = js

shouldInline :: JS -> Bool
shouldInline (JSVar _) = True
shouldInline (JSAccessor _ val) = shouldInline val
shouldInline _ = False

inlineVariables :: JS -> JS
inlineVariables = everywhere (mkT removeFromBlock)
  where
  removeFromBlock :: JS -> JS
  removeFromBlock (JSBlock sts) = JSBlock (go sts)
  removeFromBlock js = js
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var js : sts) | shouldInline js = go (replaceIdent var js sts)
  go (s:sts) = s : go sts

etaConvert :: JS -> JS
etaConvert = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing [ident] (JSBlock body)) [arg])]) | shouldInline arg = JSBlock (replaceIdent ident arg body)
  convert js = js
