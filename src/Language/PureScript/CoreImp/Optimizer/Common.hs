-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreImp.Optimizer.Common
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common functions used by the various optimizer phases
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreImp.Optimizer.Common where

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Traversals

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

removeFromBlock :: ([Statement a] -> [Statement a]) -> ([LoopStatement a] -> [LoopStatement a]) -> Decl a -> Decl a
removeFromBlock fs fls = go
  where
  (go, _, _, _) = everywhere decl expr stmnt id
  decl (Function ann name args body) = Function ann name args (fs body)
  decl other = other
  expr (AnonFunction ann args body) = AnonFunction ann args (fs body)
  expr other = other
  stmnt (Loop ann cond body) = Loop ann cond (fls body)
  stmnt (IfElse ann conf thens elses) = IfElse ann conf (fs thens) (fmap fs elses)
  stmnt other = other
