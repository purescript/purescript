module Data.Semiring
  ( class Semiring, add, (+), zero, mul, (*), one
  , class SemiringRecord, addRecord, mulRecord, oneRecord, zeroRecord
  ) where

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))

-- | The `Semiring` class is for types that support an addition and
-- | multiplication operation.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Commutative monoid under addition:
-- |   - Associativity: `(a + b) + c = a + (b + c)`
-- |   - Identity: `zero + a = a + zero = a`
-- |   - Commutative: `a + b = b + a`
-- | - Monoid under multiplication:
-- |   - Associativity: `(a * b) * c = a * (b * c)`
-- |   - Identity: `one * a = a * one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
-- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
-- | - Annihilation: `zero * a = a * zero = zero`
-- |
-- | **Note:** The `Number` and `Int` types are not fully law abiding
-- | members of this class hierarchy due to the potential for arithmetic
-- | overflows, and in the case of `Number`, the presence of `NaN` and
-- | `Infinity` values. The behaviour is unspecified in these cases.
class Semiring a where
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

infixl 6 add as +
infixl 7 mul as *

instance semiringInt :: Semiring Int where
  add = intAdd
  zero = 0
  mul = intMul
  one = 1

instance semiringNumber :: Semiring Number where
  add = numAdd
  zero = 0.0
  mul = numMul
  one = 1.0

instance semiringFn :: Semiring b => Semiring (a -> b) where
  add f g x = f x + g x
  zero = \_ -> zero
  mul f g x = f x * g x
  one = \_ -> one

instance semiringUnit :: Semiring Unit where
  add _ _ = unit
  zero = unit
  mul _ _ = unit
  one = unit

instance semiringRecord :: (RL.RowToList row list, SemiringRecord list row row) => Semiring (Record row) where
  add = addRecord (RLProxy :: RLProxy list)
  mul = mulRecord (RLProxy :: RLProxy list)
  one = oneRecord (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  zero = zeroRecord (RLProxy :: RLProxy list) (RProxy :: RProxy row)

foreign import intAdd :: Int -> Int -> Int
foreign import intMul :: Int -> Int -> Int
foreign import numAdd :: Number -> Number -> Number
foreign import numMul :: Number -> Number -> Number

-- | A class for records where all fields have `Semiring` instances, used to
-- | implement the `Semiring` instance for records.
class SemiringRecord rowlist row subrow | rowlist -> subrow where
  addRecord :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  mulRecord :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  oneRecord :: RLProxy rowlist -> RProxy row -> Record subrow
  zeroRecord :: RLProxy rowlist -> RProxy row -> Record subrow

instance semiringRecordNil :: SemiringRecord RL.Nil row () where
  addRecord  _ _ _ = {}
  mulRecord  _ _ _ = {}
  oneRecord  _ _ = {}
  zeroRecord _ _ = {}

instance semiringRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , SemiringRecord rowlistTail row subrowTail
       , Semiring focus
       )
    => SemiringRecord (RL.Cons key focus rowlistTail) row subrow where
  addRecord _ ra rb = insert (get ra + get rb) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      tail = addRecord (RLProxy :: RLProxy rowlistTail) ra rb
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow

  mulRecord _ ra rb = insert (get ra * get rb) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      tail = mulRecord (RLProxy :: RLProxy rowlistTail) ra rb
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow

  oneRecord _ _ = insert one tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      tail = oneRecord (RLProxy :: RLProxy rowlistTail) (RProxy :: RProxy row)
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow

  zeroRecord _ _ = insert zero tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      tail = zeroRecord (RLProxy :: RLProxy rowlistTail) (RProxy :: RProxy row)
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
