module Data.Monoid.Disj where

import Prelude

import Data.Eq (class Eq1)
import Data.HeytingAlgebra (ff, tt)
import Data.Ord (class Ord1)

-- | Monoid and semigroup for disjunction.
-- |
-- | ``` purescript
-- | Disj x <> Disj y == Disj (x || y)
-- | (mempty :: Disj _) == Disj bottom
-- | ```
newtype Disj a = Disj a

derive newtype instance eqDisj :: Eq a => Eq (Disj a)
derive instance eq1Disj :: Eq1 Disj

derive newtype instance ordDisj :: Ord a => Ord (Disj a)
derive instance ord1Disj :: Ord1 Disj

derive newtype instance boundedDisj :: Bounded a => Bounded (Disj a)

instance showDisj :: Show a => Show (Disj a) where
  show (Disj a) = "(Disj " <> show a <> ")"

derive instance functorDisj :: Functor Disj

instance applyDisj :: Apply Disj where
  apply (Disj f) (Disj x) = Disj (f x)

instance applicativeDisj :: Applicative Disj where
  pure = Disj

instance bindDisj :: Bind Disj where
  bind (Disj x) f = f x

instance monadDisj :: Monad Disj

instance semigroupDisj :: HeytingAlgebra a => Semigroup (Disj a) where
  append (Disj a) (Disj b) = Disj (disj a b)

instance monoidDisj :: HeytingAlgebra a => Monoid (Disj a) where
  mempty = Disj ff

instance semiringDisj :: HeytingAlgebra a => Semiring (Disj a) where
  zero = Disj ff
  one = Disj tt
  add (Disj a) (Disj b) = Disj (disj a b)
  mul (Disj a) (Disj b) = Disj (conj a b)
