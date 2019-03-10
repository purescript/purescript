module Data.Monoid
  ( class Monoid, mempty
  , power
  , guard
  , module Data.Semigroup
  , class MonoidRecord, memptyRecord
  ) where

import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.EuclideanRing (mod, (/))
import Data.Ord ((<=))
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, class SemigroupRecord, (<>))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeSet)
import Type.Data.RowList (RLProxy(..))

-- | A `Monoid` is a `Semigroup` with a value `mempty`, which is both a
-- | left and right unit for the associative operation `<>`:
-- |
-- | - Left unit: `(mempty <> x) = x`
-- | - Right unit: `(x <> mempty) = x`
-- |
-- | `Monoid`s are commonly used as the result of fold operations, where
-- | `<>` is used to combine individual results, and `mempty` gives the result
-- | of folding an empty collection of elements.
class Semigroup m <= Monoid m where
  mempty :: m

instance monoidUnit :: Monoid Unit where
  mempty = unit

instance monoidOrdering :: Monoid Ordering where
  mempty = EQ

instance monoidFn :: Monoid b => Monoid (a -> b) where
  mempty _ = mempty

instance monoidString :: Monoid String where
  mempty = ""

instance monoidArray :: Monoid (Array a) where
  mempty = []

instance monoidRecord :: (RL.RowToList row list, MonoidRecord list row row) => Monoid (Record row) where
  mempty = memptyRecord (RLProxy :: RLProxy list)

-- | Append a value to itself a certain number of times. For the
-- | `Multiplicative` type, and for a non-negative power, this is the same as
-- | normal number exponentiation.
-- |
-- | If the second argument is negative this function will return `mempty`
-- | (*unlike* normal number exponentiation). The `Monoid` constraint alone
-- | is not enough to write a `power` function with the property that `power x
-- | n` cancels with `power x (-n)`, i.e. `power x n <> power x (-n) = mempty`.
-- | For that, we would additionally need the ability to invert elements, i.e.
-- | a Group.
power :: forall m. Monoid m => m -> Int -> m
power x = go
  where
  go :: Int -> m
  go p
    | p <= 0 = mempty
    | p == 1 = x
    | p `mod` 2 == 0 = let x' = go (p / 2) in x' <> x'
    | otherwise = let x' = go (p / 2) in x' <> x' <> x

-- | Allow or "truncate" a Monoid to its `mempty` value based on a condition.
guard :: forall m. Monoid m => Boolean -> m -> m
guard true a = a
guard false _ = mempty

-- | A class for records where all fields have `Monoid` instances, used to
-- | implement the `Monoid` instance for records.
class SemigroupRecord rowlist row subrow <= MonoidRecord rowlist row subrow | rowlist -> row subrow where
  memptyRecord :: RLProxy rowlist -> Record subrow

instance monoidRecordNil :: MonoidRecord RL.Nil row () where
  memptyRecord _ = {}

instance monoidRecordCons
    :: ( IsSymbol key
       , Monoid focus
       , Row.Cons key focus subrowTail subrow
       , MonoidRecord rowlistTail row subrowTail
       )
    => MonoidRecord (RL.Cons key focus rowlistTail) row subrow where
  memptyRecord _
    = insert mempty tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = memptyRecord (RLProxy :: RLProxy rowlistTail)
