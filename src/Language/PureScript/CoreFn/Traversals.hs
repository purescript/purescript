-- |
-- CoreFn traversal helpers
--
module Language.PureScript.CoreFn.Traversals where

import Prelude

import Control.Arrow (second, (***), (+++))
import Data.Bitraversable (bitraverse)

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

-- |
-- Apply the provided functions to the top level of AST nodes.
--
-- This function is useful as a building block for recursive functions, but
-- doesn't actually recurse itself.
--
traverseCoreFn
  :: forall f a
   . Applicative f
  => (Bind a -> f (Bind a))
  -> (Expr a -> f (Expr a))
  -> (Binder a -> f (Binder a))
  -> (CaseAlternative a -> f (CaseAlternative a))
  -> (Bind a -> f (Bind a), Expr a -> f (Expr a), Binder a -> f (Binder a), CaseAlternative a -> f (CaseAlternative a))
traverseCoreFn f g h i = (f', g', h', i')
  where
  f' (NonRec a name e) = NonRec a name <$> g e
  f' (Rec es) = Rec <$> traverse (traverse g) es

  g' (Literal ann e) = Literal ann <$> handleLiteral g e
  g' (Accessor ann prop e) = Accessor ann prop <$> g e
  g' (ObjectUpdate ann obj vs) = ObjectUpdate ann <$> g obj <*> traverse (traverse g) vs
  g' (Abs ann name e) = Abs ann name <$> g e
  g' (App ann v1 v2) = App ann <$> g v1 <*> g v2
  g' (Case ann vs alts) = Case ann <$> traverse g vs <*> traverse i alts
  g' (Let ann ds e) = Let ann <$> traverse f ds <*> g' e
  g' e = pure e

  h' (LiteralBinder a b) = LiteralBinder a <$> handleLiteral h b
  h' (NamedBinder a name b) = NamedBinder a name <$> h b
  h' (ConstructorBinder a q1 q2 bs) = ConstructorBinder a q1 q2 <$> traverse h bs
  h' b = pure b

  i' ca = CaseAlternative <$> traverse h (caseAlternativeBinders ca) <*> bitraverse (traverse $ bitraverse g g) g (caseAlternativeResult ca)

  handleLiteral withItem = \case
    ArrayLiteral ls -> ArrayLiteral <$> traverse withItem ls
    ObjectLiteral ls -> ObjectLiteral <$> traverse (traverse withItem) ls
    other -> pure other
