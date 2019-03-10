module Data.Monoid.Dual where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | The dual of a monoid.
-- |
-- | ``` purescript
-- | Dual x <> Dual y == Dual (y <> x)
-- | (mempty :: Dual _) == Dual mempty
-- | ```
newtype Dual a = Dual a

derive newtype instance eqDual :: Eq a => Eq (Dual a)
derive instance eq1Dual :: Eq1 Dual

derive newtype instance ordDual :: Ord a => Ord (Dual a)
derive instance ord1Dual :: Ord1 Dual

derive newtype instance boundedDual :: Bounded a => Bounded (Dual a)

instance showDual :: Show a => Show (Dual a) where
  show (Dual a) = "(Dual " <> show a <> ")"

derive instance functorDual :: Functor Dual

instance applyDual :: Apply Dual where
  apply (Dual f) (Dual x) = Dual (f x)

instance applicativeDual :: Applicative Dual where
  pure = Dual

instance bindDual :: Bind Dual where
  bind (Dual x) f = f x

instance monadDual :: Monad Dual

instance semigroupDual :: Semigroup a => Semigroup (Dual a) where
  append (Dual x) (Dual y) = Dual (y <> x)

instance monoidDual :: Monoid a => Monoid (Dual a) where
  mempty = Dual mempty
