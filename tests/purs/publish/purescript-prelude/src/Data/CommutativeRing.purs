module Data.CommutativeRing
  ( class CommutativeRing
  , module Data.Ring
  , module Data.Semiring
  , class CommutativeRingRecord
  ) where

import Data.Ring (class Ring, class RingRecord)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit)
import Prim.Row as Row
import Prim.RowList as RL

-- | The `CommutativeRing` class is for rings where multiplication is
-- | commutative.
-- |
-- | Instances must satisfy the following law in addition to the `Ring`
-- | laws:
-- |
-- | - Commutative multiplication: `a * b = b * a`
class Ring a <= CommutativeRing a

instance commutativeRingInt :: CommutativeRing Int
instance commutativeRingNumber :: CommutativeRing Number
instance commutativeRingUnit :: CommutativeRing Unit
instance commutativeRingFn :: CommutativeRing b => CommutativeRing (a -> b)
instance commutativeRingRecord :: (RL.RowToList row list, CommutativeRingRecord list row row) => CommutativeRing (Record row)

-- | A class for records where all fields have `CommutativeRing` instances, used
-- | to implement the `CommutativeRing` instance for records.
class RingRecord rowlist row subrow <= CommutativeRingRecord rowlist row subrow | rowlist -> subrow

instance commutativeRingRecordNil :: CommutativeRingRecord RL.Nil row ()

instance commutativeRingRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , CommutativeRingRecord rowlistTail row subrowTail
       , CommutativeRing focus
       )
    => CommutativeRingRecord (RL.Cons key focus rowlistTail) row subrow
