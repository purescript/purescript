module Data.Monoid.Additive where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | Monoid and semigroup for semirings under addition.
-- |
-- | ``` purescript
-- | Additive x <> Additive y == Additive (x + y)
-- | (mempty :: Additive _) == Additive zero
-- | ```
newtype Additive a = Additive a

derive newtype instance eqAdditive :: Eq a => Eq (Additive a)
derive instance eq1Additive :: Eq1 Additive

derive newtype instance ordAdditive :: Ord a => Ord (Additive a)
derive instance ord1Additive :: Ord1 Additive

derive newtype instance boundedAdditive :: Bounded a => Bounded (Additive a)

instance showAdditive :: Show a => Show (Additive a) where
  show (Additive a) = "(Additive " <> show a <> ")"

derive instance functorAdditive :: Functor Additive

instance applyAdditive :: Apply Additive where
  apply (Additive f) (Additive x) = Additive (f x)

instance applicativeAdditive :: Applicative Additive where
  pure = Additive

instance bindAdditive :: Bind Additive where
  bind (Additive x) f = f x

instance monadAdditive :: Monad Additive

instance semigroupAdditive :: Semiring a => Semigroup (Additive a) where
  append (Additive a) (Additive b) = Additive (a + b)

instance monoidAdditive :: Semiring a => Monoid (Additive a) where
  mempty = Additive zero
