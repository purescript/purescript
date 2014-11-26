-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Traversals
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | CoreFn traversal helpers
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Traversals where

import Control.Arrow (second, (***), (+++))

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals

everywhereOnValues :: (Bind -> Bind) ->
                      (Expr -> Expr) ->
                      (Binder -> Binder) ->
                      (Bind -> Bind, Expr -> Expr, Binder -> Binder)
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Bind -> Bind
  f' (NotRec name e) = f (NotRec name (g' e))
  f' (Rec es) = f (Rec (map (second g') es))

  g' :: Expr -> Expr
  g' (Literal e) = g (Literal (handleLiteral g' e))
  g' (Accessor prop e) = g (Accessor prop (g' e))
  g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map (fmap g') vs))
  g' (Abs name e) = g (Abs name (g' e))
  g' (App v1 v2) = g (App (g' v1) (g' v2))
  g' (Case vs alts) = g (Case (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue e ty) = g (TypedValue (g' e) ty)
  g' (Let ds e) = g (Let (map f' ds) (g' e))
  g' (Meta m e) = g (Meta m (g' e))
  g' e = g e

  h' :: Binder -> Binder
  h' (LiteralBinder b) = h (LiteralBinder (handleLiteral h' b))
  h' (NamedBinder name b) = h (NamedBinder name (h' b))
  h' b = h b

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = (map (g' *** g') +++ g') (caseAlternativeResult ca)
       }

  handleLiteral :: (a -> a) -> Literal a -> Literal a
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral (map i ls)
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral (map (fmap i) ls)
  handleLiteral _ other = other

everythingOnValues :: (r -> r -> r) ->
                      (Bind -> r) ->
                      (Expr -> r) ->
                      (Binder -> r) ->
                      (CaseAlternative -> r) ->
                      (Bind -> r, Expr -> r, Binder -> r, CaseAlternative -> r)
everythingOnValues (<>) f g h i = (f', g', h', i')
  where
  f' b@(NotRec _ e) = f b <> g' e
  f' b@(Rec es) = foldl (<>) (f b) (map (g' . snd) es)

  g' v@(Literal l) = foldl (<>) (g v) (map g' (extractLiteral l))
  g' v@(Accessor _ e1) = g v <> g' e1
  g' v@(ObjectUpdate obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(Abs _ e1) = g v <> g' e1
  g' v@(App e1 e2) = g v <> g' e1 <> g' e2
  g' v@(Case vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue e1 _) = g v <> g' e1
  g' v@(Let ds e1) = foldl (<>) (g v) (map f' ds) <> g' e1
  g' v@(Meta _ e1) = g v <> g' e1
  g' v = g v

  h' b@(LiteralBinder l) = foldl (<>) (h b) (map h' (extractLiteral l))
  h' b@(ConstructorBinder _ _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(NamedBinder _ b1) = h b <> h' b1
  h' b = h b

  i' ca@(CaseAlternative bs (Right val)) = foldl (<>) (i ca) (map h' bs) <> g' val
  i' ca@(CaseAlternative bs (Left gs)) = foldl (<>) (i ca) (map h' bs ++ concatMap (\(grd, val) -> [g' grd, g' val]) gs)

  extractLiteral (ArrayLiteral xs) = xs
  extractLiteral (ObjectLiteral xs) = map snd xs
  extractLiteral _ = []
