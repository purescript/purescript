module Data.Monoid.Multiplicative where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | Monoid and semigroup for semirings under multiplication.
-- |
-- | ``` purescript
-- | Multiplicative x <> Multiplicative y == Multiplicative (x * y)
-- | (mempty :: Multiplicative _) == Multiplicative one
-- | ```
newtype Multiplicative a = Multiplicative a

derive newtype instance eqMultiplicative :: Eq a => Eq (Multiplicative a)
derive instance eq1Multiplicative :: Eq1 Multiplicative

derive newtype instance ordMultiplicative :: Ord a => Ord (Multiplicative a)
derive instance ord1Multiplicative :: Ord1 Multiplicative

derive newtype instance boundedMultiplicative :: Bounded a => Bounded (Multiplicative a)

instance showMultiplicative :: Show a => Show (Multiplicative a) where
  show (Multiplicative a) = "(Multiplicative " <> show a <> ")"

derive instance functorMultiplicative :: Functor Multiplicative

instance applyMultiplicative :: Apply Multiplicative where
  apply (Multiplicative f) (Multiplicative x) = Multiplicative (f x)

instance applicativeMultiplicative :: Applicative Multiplicative where
  pure = Multiplicative

instance bindMultiplicative :: Bind Multiplicative where
  bind (Multiplicative x) f = f x

instance monadMultiplicative :: Monad Multiplicative

instance semigroupMultiplicative :: Semiring a => Semigroup (Multiplicative a) where
  append (Multiplicative a) (Multiplicative b) = Multiplicative (a * b)

instance monoidMultiplicative :: Semiring a => Monoid (Multiplicative a) where
  mempty = Multiplicative one
