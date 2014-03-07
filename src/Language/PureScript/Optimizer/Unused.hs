-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer.Unused
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Removes unused variables
--
-----------------------------------------------------------------------------

module Language.PureScript.Optimizer.Unused (
  removeUnusedVariables
) where

import Data.Generics

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Optimizer.Common

removeUnusedVariables :: JS -> JS
removeUnusedVariables = everywhere (mkT $ removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var _ : sts) | not (isUsed var sts) = go sts
  go (s:sts) = s : go sts
