module Data.Monoid.Conj where

import Prelude

import Data.Eq (class Eq1)
import Data.HeytingAlgebra (ff, tt)
import Data.Ord (class Ord1)

-- | Monoid and semigroup for conjunction.
-- |
-- | ``` purescript
-- | Conj x <> Conj y == Conj (x && y)
-- | (mempty :: Conj _) == Conj tt
-- | ```
newtype Conj a = Conj a

derive newtype instance eqConj :: Eq a => Eq (Conj a)
derive instance eq1Conj :: Eq1 Conj

derive newtype instance ordConj :: Ord a => Ord (Conj a)
derive instance ord1Conj :: Ord1 Conj

derive newtype instance boundedConj :: Bounded a => Bounded (Conj a)

instance showConj :: (Show a) => Show (Conj a) where
  show (Conj a) = "(Conj " <> show a <> ")"

derive instance functorConj :: Functor Conj

instance applyConj :: Apply Conj where
  apply (Conj f) (Conj x) = Conj (f x)

instance applicativeConj :: Applicative Conj where
  pure = Conj

instance bindConj :: Bind Conj where
  bind (Conj x) f = f x

instance monadConj :: Monad Conj

instance semigroupConj :: HeytingAlgebra a => Semigroup (Conj a) where
  append (Conj a) (Conj b) = Conj (conj a b)

instance monoidConj :: HeytingAlgebra a => Monoid (Conj a) where
  mempty = Conj tt

instance semiringConj :: HeytingAlgebra a => Semiring (Conj a) where
  zero = Conj tt
  one = Conj ff
  add (Conj a) (Conj b) = Conj (conj a b)
  mul (Conj a) (Conj b) = Conj (disj a b)
