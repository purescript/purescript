-- | Optimizer steps for simplifying JavaScript blocks
module Language.PureScript.CoreImp.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Prelude.Compat

import Language.PureScript.CoreImp.AST

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: AST -> AST
collapseNestedBlocks = everywhere collapse where
  collapse :: AST -> AST
  collapse (Block ss sts) = Block ss (concatMap go sts)
  collapse js = js
  
  go :: AST -> [AST]
  go (Block _ sts) = sts
  go s = [s]

collapseNestedIfs :: AST -> AST
collapseNestedIfs = everywhere collapse where
  collapse :: AST -> AST
  collapse (IfElse _ (BooleanLiteral _ True) (Block _ [js]) _) = js
  collapse (IfElse s1 cond1 (Block _ [IfElse s2 cond2 body Nothing]) Nothing) =
      IfElse s1 (Binary s2 And cond1 cond2) body Nothing
  collapse js = js
