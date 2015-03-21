-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreImp.Traversals
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | CoreImp traversal helpers
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreImp.Traversals where

import Control.Arrow (second)
import Data.Maybe (fromMaybe)
import Language.PureScript.Core.Literals
import Language.PureScript.CoreImp.AST

everywhereOnValues :: (Decl a -> Decl a) ->
                      (Expr a -> Expr a) ->
                      (Statement a -> Statement a) ->
                      (LoopStatement a -> LoopStatement a) ->
                      (Decl a -> Decl a, Expr a -> Expr a, Statement a -> Statement a, LoopStatement a -> LoopStatement a)
everywhereOnValues f g h i = (f', g', h', i')
  where
  f' (Function ann name args ss) = f (Function ann name args (map h' ss))
  f' (VarDecl ann name e) = f (VarDecl ann name (g' e))
  f' d@(Constructor{}) = f d

  g' (Literal ann l) = g (Literal ann (modifyLiteral g' l))
  g' (Accessor ann e1 e2) = g (Accessor ann (g' e1) (g' e2))
  g' (Indexer ann e1 e2) = g (Indexer ann (g' e1) (g' e2))
  g' (AnonFunction ann args ss) = g (AnonFunction ann args (map h' ss))
  g' (App ann e1 es) = g (App ann (g' e1) (map g' es))
  g' e@(Var{}) = g e
  g' (ObjectUpdate ann e1 es) = g (ObjectUpdate ann (g' e1) (map (second g') es))
  g' (UnaryOp ann op e1) = g (UnaryOp ann op (g' e1))
  g' (BinaryOp ann op e1 e2) = g (BinaryOp ann op (g' e1) (g' e2))
  g' (IsTagOf ann tag e1) = g (IsTagOf ann tag (g' e1))

  h' (Expr e) = h (Expr (g' e))
  h' (Decl d) = h (Decl (f' d))
  h' (Assignment ann e1 e2) = h (Assignment ann (g' e1) (g' e2))
  h' (Loop ann e ls) = h (Loop ann (g' e) (map i' ls))
  h' (IfElse ann e ss mss) = h (IfElse ann (g' e) (map h' ss) (fmap (map h') mss))
  h' (Return ann e) = h (Return ann (g' e))
  h' s@(Throw{}) = h s
  h' (Label ann lbl s) = h (Label ann lbl (h' s))
  h' s@(Comment{}) = h s

  i' l@(Break{}) = i l
  i' l@(Continue{}) = i l
  i' (Statement s) = i (Statement (h' s))

everythingOnValues :: (r -> r -> r) ->
                      (Decl a -> r) ->
                      (Expr a -> r) ->
                      (Statement a -> r) ->
                      (LoopStatement a -> r) ->
                      (Decl a -> r, Expr a -> r, Statement a -> r, LoopStatement a -> r)
everythingOnValues (<>) f g h i = (f', g', h', i')
  where
  f' d@(Function _ _ _ ss) = foldl (<>) (f d) (map h' ss)
  f' d@(VarDecl _ _ e) = f d <> g' e
  f' d@(Constructor{}) = f d

  g' e@(Literal _ l) = foldl (<>) (g e) (map g' (extractLiteral l))
  g' e@(Accessor _ e1 e2) = g e <> g' e1 <> g' e2
  g' e@(Indexer _ e1 e2) = g e <> g' e1 <> g' e2
  g' e@(AnonFunction _ _ ss) = foldl (<>) (g e) (map h' ss)
  g' e@(App _ e1 es) = foldl (<>) (g e <> g e1) (map g' es)
  g' e@(Var{}) = g e
  g' e@(ObjectUpdate _ e1 es) = foldl (<>) (g e <> g' e1) (map (g' . snd) es)
  g' e@(UnaryOp _ _ e1) = g e <> g' e1
  g' e@(BinaryOp _ _ e1 e2) = g e <> g' e1 <> g' e2
  g' e@(IsTagOf _ _ e1) = g e <> g' e1

  h' s@(Expr e) = h s <> g' e
  h' s@(Decl d) = h s <> f' d
  h' s@(Assignment _ e1 e2) = h s <> g' e1 <> g' e2
  h' s@(Loop _ e ls) = foldl (<>) (h s <> g' e) (map i' ls)
  h' s@(IfElse _ e ss mss) = foldl (<>) (h s <> g' e) (map h' (ss ++ fromMaybe [] mss))
  h' s@(Return _ e) = h s <> g' e
  h' s@(Throw{}) = h s
  h' s@(Label _ _ s2) = h s <> h' s2
  h' s@(Comment{}) = h s

  i' l@(Break{}) = i l
  i' l@(Continue{}) = i l
  i' l@(Statement s) = i l <> h' s
