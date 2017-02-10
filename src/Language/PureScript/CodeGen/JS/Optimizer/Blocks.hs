-- |
-- Optimizer steps for simplifying JavaScript blocks
--
module Language.PureScript.CodeGen.JS.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.JS.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: JS -> JS
collapseNestedBlocks = everywhereOnJS collapse
  where
  collapse :: JS -> JS
  collapse (JSBlock ss sts) = JSBlock ss (concatMap go sts)
  collapse js = js
  go :: JS -> [JS]
  go (JSBlock _ sts) = sts
  go s = [s]

collapseNestedIfs :: JS -> JS
collapseNestedIfs = everywhereOnJS collapse
  where
  collapse :: JS -> JS
  collapse (JSIfElse s1 cond1 (JSBlock _ [JSIfElse s2 cond2 body Nothing]) Nothing) =
      JSIfElse s1 (JSBinary s2 And cond1 cond2) body Nothing
  collapse js = js
