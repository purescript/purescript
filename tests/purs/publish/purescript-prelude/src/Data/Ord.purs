module Data.Ord
  ( class Ord, compare
  , class Ord1, compare1
  , lessThan, (<)
  , lessThanOrEq, (<=)
  , greaterThan, (>)
  , greaterThanOrEq, (>=)
  , comparing
  , min, max
  , clamp
  , between
  , abs
  , signum
  , module Data.Ordering
  , class OrdRecord, compareRecord
  ) where

import Data.Eq (class Eq, class Eq1, class EqRecord, (/=))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Ord.Unsafe (unsafeCompare)
import Data.Ordering (Ordering(..))
import Data.Ring (class Ring, zero, one, negate)
import Data.Unit (Unit)
import Data.Void (Void)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Data.RowList (RLProxy(..))

-- | The `Ord` type class represents types which support comparisons with a
-- | _total order_.
-- |
-- | `Ord` instances should satisfy the laws of total orderings:
-- |
-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
class Eq a <= Ord a where
  compare :: a -> a -> Ordering

instance ordBoolean :: Ord Boolean where
  compare = unsafeCompare

instance ordInt :: Ord Int where
  compare = unsafeCompare

instance ordNumber :: Ord Number where
  compare = unsafeCompare

instance ordString :: Ord String where
  compare = unsafeCompare

instance ordChar :: Ord Char where
  compare = unsafeCompare

instance ordUnit :: Ord Unit where
  compare _ _ = EQ

instance ordVoid :: Ord Void where
  compare _ _ = EQ

instance ordArray :: Ord a => Ord (Array a) where
  compare = \xs ys -> compare 0 (ordArrayImpl toDelta xs ys)
    where
    toDelta x y =
      case compare x y of
        EQ -> 0
        LT -> 1
        GT -> -1

foreign import ordArrayImpl :: forall a. (a -> a -> Int) -> Array a -> Array a -> Int

instance ordOrdering :: Ord Ordering where
  compare LT LT = EQ
  compare EQ EQ = EQ
  compare GT GT = EQ
  compare LT _  = LT
  compare EQ LT = GT
  compare EQ GT = LT
  compare GT _  = GT

-- | Test whether one value is _strictly less than_ another.
lessThan :: forall a. Ord a => a -> a -> Boolean
lessThan a1 a2 = case a1 `compare` a2 of
  LT -> true
  _ -> false

-- | Test whether one value is _strictly greater than_ another.
greaterThan :: forall a. Ord a => a -> a -> Boolean
greaterThan a1 a2 = case a1 `compare` a2 of
  GT -> true
  _ -> false

-- | Test whether one value is _non-strictly less than_ another.
lessThanOrEq :: forall a. Ord a => a -> a -> Boolean
lessThanOrEq a1 a2 = case a1 `compare` a2 of
  GT -> false
  _ -> true

-- | Test whether one value is _non-strictly greater than_ another.
greaterThanOrEq :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEq a1 a2 = case a1 `compare` a2 of
  LT -> false
  _ -> true

infixl 4 lessThan as <
infixl 4 lessThanOrEq as <=
infixl 4 greaterThan as >
infixl 4 greaterThanOrEq as >=

-- | Compares two values by mapping them to a type with an `Ord` instance.
comparing :: forall a b. Ord b => (a -> b) -> (a -> a -> Ordering)
comparing f x y = compare (f x) (f y)

-- | Take the minimum of two values. If they are considered equal, the first
-- | argument is chosen.
min :: forall a. Ord a => a -> a -> a
min x y =
  case compare x y of
    LT -> x
    EQ -> x
    GT -> y

-- | Take the maximum of two values. If they are considered equal, the first
-- | argument is chosen.
max :: forall a. Ord a => a -> a -> a
max x y =
  case compare x y of
    LT -> y
    EQ -> x
    GT -> x

-- | Clamp a value between a minimum and a maximum. For example:
-- |
-- | ``` purescript
-- | let f = clamp 0 10
-- | f (-5) == 0
-- | f 5    == 5
-- | f 15   == 10
-- | ```
clamp :: forall a. Ord a => a -> a -> a -> a
clamp low hi x = min hi (max low x)

-- | Test whether a value is between a minimum and a maximum (inclusive).
-- | For example:
-- |
-- | ``` purescript
-- | let f = between 0 10
-- | f 0    == true
-- | f (-5) == false
-- | f 5    == true
-- | f 10   == true
-- | f 15   == false
-- | ```
between :: forall a. Ord a => a -> a -> a -> Boolean
between low hi x
  | x < low = false
  | x > hi = false
  | true = true

-- | The absolute value function. `abs x` is defined as `if x >= zero then x
-- | else negate x`.
abs :: forall a. Ord a => Ring a => a -> a
abs x = if x >= zero then x else negate x

-- | The sign function; always evaluates to either `one` or `negate one`. For
-- | any `x`, we should have `signum x * abs x == x`.
signum :: forall a. Ord a => Ring a => a -> a
signum x = if x >= zero then one else negate one

-- | The `Ord1` type class represents totally ordered type constructors.
class Eq1 f <= Ord1 f where
  compare1 :: forall a. Ord a => f a -> f a -> Ordering

instance ord1Array :: Ord1 Array where
  compare1 = compare

class EqRecord rowlist row <= OrdRecord rowlist row where
  compareRecord :: RLProxy rowlist -> Record row -> Record row -> Ordering

instance ordRecordNil :: OrdRecord RL.Nil row where
  compareRecord _ _ _ = EQ

instance ordRecordCons
    :: ( OrdRecord rowlistTail row
       , Row.Cons key focus rowTail row
       , IsSymbol key
       , Ord focus
       )
    => OrdRecord (RL.Cons key focus rowlistTail) row where
  compareRecord _ ra rb
    = if left /= EQ
        then left
        else compareRecord (RLProxy :: RLProxy rowlistTail) ra rb
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus
      left = unsafeGet' key ra `compare` unsafeGet' key rb

instance ordRecord
    :: ( RL.RowToList row list
       , OrdRecord list row
       )
    => Ord (Record row) where
  compare = compareRecord (RLProxy :: RLProxy list)

