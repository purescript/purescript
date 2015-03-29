-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreImp.Optimizer.Nesting
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Optimizer steps for simplifying nested statements
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreImp.Optimizer.Nesting where

import Language.PureScript.Core.Ann
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Operators
import Language.PureScript.CoreImp.Traversals

collapseNestedIfs :: Decl Ann -> Decl Ann
collapseNestedIfs = go
  where
  (go, _, _, _) = everywhere id id collapse id
  collapse :: Statement Ann -> Statement Ann
  collapse (IfElse ann cond1 [IfElse _ cond2 body Nothing] Nothing) =
      IfElse ann (BinaryOp nullAnn And cond1 cond2) body Nothing
  collapse other = other
