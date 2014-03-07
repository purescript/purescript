-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer.Blocks
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Optimizer steps for simplifying Javascript blocks
--
-----------------------------------------------------------------------------

module Language.PureScript.Optimizer.Blocks (
  collapseNestedBlocks
) where

import Data.Generics

import Language.PureScript.CodeGen.JS.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: JS -> JS
collapseNestedBlocks = everywhere (mkT collapse)
  where
  collapse :: JS -> JS
  collapse (JSBlock sts) = JSBlock (concatMap go sts)
  collapse js = js
  go :: JS -> [JS]
  go (JSBlock sts) = sts
  go s = [s]
