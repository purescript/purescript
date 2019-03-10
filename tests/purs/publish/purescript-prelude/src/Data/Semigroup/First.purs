module Data.Semigroup.First where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | Semigroup where `append` always takes the first option.
-- |
-- | ``` purescript
-- | First x <> First y == First x
-- | ```
newtype First a = First a

derive newtype instance eqFirst :: Eq a => Eq (First a)
derive instance eq1First :: Eq1 First

derive newtype instance ordFirst :: Ord a => Ord (First a)
derive instance ord1First :: Ord1 First

derive newtype instance boundedFirst :: Bounded a => Bounded (First a)

instance showFirst :: Show a => Show (First a) where
  show (First a) = "(First " <> show a <> ")"

derive instance functorFirst :: Functor First

instance applyFirst :: Apply First where
  apply (First f) (First x) = First (f x)

instance applicativeFirst :: Applicative First where
  pure = First

instance bindFirst :: Bind First where
  bind (First x) f = f x

instance monadFirst :: Monad First

instance semigroupFirst :: Semigroup (First a) where
  append x _ = x
