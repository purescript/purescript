module Data.Semigroup.Last where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | Semigroup where `append` always takes the second option.
-- |
-- | ``` purescript
-- | Last x <> Last y == Last y
-- | ```
newtype Last a = Last a

derive newtype instance eqLast :: Eq a => Eq (Last a)
derive instance eq1Last :: Eq1 Last

derive newtype instance ordLast :: Ord a => Ord (Last a)
derive instance ord1Last :: Ord1 Last

derive newtype instance boundedLast :: Bounded a => Bounded (Last a)

instance showLast :: Show a => Show (Last a) where
  show (Last a) = "(Last " <> show a <> ")"

derive instance functorLast :: Functor Last

instance applyLast :: Apply Last where
  apply (Last f) (Last x) = Last (f x)

instance applicativeLast :: Applicative Last where
  pure = Last

instance bindLast :: Bind Last where
  bind (Last x) f = f x

instance monadLast :: Monad Last

instance semigroupLast :: Semigroup (Last a) where
  append _ x = x
