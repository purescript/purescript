-- |
-- CoreFn traversal helpers
--
module Language.PureScript.CoreFn.Traversals where

import Prelude.Compat

import Control.Arrow (second, (***), (+++))

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr

everywhereOnValues :: (Bind a -> Bind a) ->
                      (Expr a -> Expr a) ->
                      (Binder a -> Binder a) ->
                      (Bind a -> Bind a, Expr a -> Expr a, Binder a -> Binder a)
everywhereOnValues f g h = (f', g', h')
  where
  f' (NonRec a name e) = f (NonRec a name (g' e))
  f' (Rec es) = f (Rec (map (second g') es))

  g' (Literal ann e) = g (Literal ann (handleLiteral g' e))
  g' (Accessor ann prop e) = g (Accessor ann prop (g' e))
  g' (ObjectUpdate ann obj vs) = g (ObjectUpdate ann (g' obj) (map (fmap g') vs))
  g' (Abs ann name e) = g (Abs ann name (g' e))
  g' (App ann v1 v2) = g (App ann (g' v1) (g' v2))
  g' (Case ann vs alts) = g (Case ann (map g' vs) (map handleCaseAlternative alts))
  g' (Let ann ds e) = g (Let ann (map f' ds) (g' e))
  g' e = g e

  h' (LiteralBinder a b) = h (LiteralBinder a (handleLiteral h' b))
  h' (NamedBinder a name b) = h (NamedBinder a name (h' b))
  h' (ConstructorBinder a q1 q2 bs) = h (ConstructorBinder a q1 q2 (map h' bs))
  h' b = h b

  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = (map (g' *** g') +++ g') (caseAlternativeResult ca)
       }

  handleLiteral :: (a -> a) -> Literal a -> Literal a
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral (map i ls)
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral (map (fmap i) ls)
  handleLiteral _ other = other

everythingOnValues :: (r -> r -> r) ->
                      (Bind a -> r) ->
                      (Expr a -> r) ->
                      (Binder a -> r) ->
                      (CaseAlternative a -> r) ->
                      (Bind a -> r, Expr a -> r, Binder a -> r, CaseAlternative a -> r)
everythingOnValues (<>) f g h i = (f', g', h', i')
  where
  f' b@(NonRec _ _ e) = f b <> g' e
  f' b@(Rec es) = foldl (<>) (f b) (map (g' . snd) es)

  g' v@(Literal _ l) = foldl (<>) (g v) (map g' (extractLiteral l))
  g' v@(Accessor _ _ e1) = g v <> g' e1
  g' v@(ObjectUpdate _ obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(Abs _ _ e1) = g v <> g' e1
  g' v@(App _ e1 e2) = g v <> g' e1 <> g' e2
  g' v@(Case _ vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(Let _ ds e1) = foldl (<>) (g v) (map f' ds) <> g' e1
  g' v = g v

  h' b@(LiteralBinder _ l) = foldl (<>) (h b) (map h' (extractLiteral l))
  h' b@(ConstructorBinder _ _ _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(NamedBinder _ _ b1) = h b <> h' b1
  h' b = h b

  i' ca@(CaseAlternative bs (Right val)) = foldl (<>) (i ca) (map h' bs) <> g' val
  i' ca@(CaseAlternative bs (Left gs)) = foldl (<>) (i ca) (map h' bs ++ concatMap (\(grd, val) -> [g' grd, g' val]) gs)

  extractLiteral (ArrayLiteral xs) = xs
  extractLiteral (ObjectLiteral xs) = map snd xs
  extractLiteral _ = []
