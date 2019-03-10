module Data.HeytingAlgebra
  ( class HeytingAlgebra, tt, ff, implies, conj, disj, not, (&&), (||)
  , class HeytingAlgebraRecord, ffRecord, ttRecord, impliesRecord, conjRecord, disjRecord, notRecord
  ) where

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))

-- | The `HeytingAlgebra` type class represents types that are bounded lattices with
-- | an implication operator such that the following laws hold:
-- |
-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
-- | - Identity:
-- |   - `a || ff = a`
-- |   - `a && tt = a`
-- | - Implication:
-- |   - ``a `implies` a = tt``
-- |   - ``a && (a `implies` b) = a && b``
-- |   - ``b && (a `implies` b) = b``
-- |   - ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
-- | - Complemented:
-- |   - ``not a = a `implies` ff``
class HeytingAlgebra a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

infixr 3 conj as &&
infixr 2 disj as ||

instance heytingAlgebraBoolean :: HeytingAlgebra Boolean where
  ff = false
  tt = true
  implies a b = not a || b
  conj = boolConj
  disj = boolDisj
  not = boolNot

instance heytingAlgebraUnit :: HeytingAlgebra Unit where
  ff = unit
  tt = unit
  implies _ _ = unit
  conj _ _ = unit
  disj _ _ = unit
  not _ = unit

instance heytingAlgebraFunction :: HeytingAlgebra b => HeytingAlgebra (a -> b) where
  ff _ = ff
  tt _ = tt
  implies f g a = f a `implies` g a
  conj f g a = f a && g a
  disj f g a = f a || g a
  not f a = not (f a)

instance heytingAlgebraRecord :: (RL.RowToList row list, HeytingAlgebraRecord list row row) => HeytingAlgebra (Record row) where
  ff = ffRecord  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  tt = ttRecord  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  conj = conjRecord  (RLProxy :: RLProxy list)
  disj = disjRecord  (RLProxy :: RLProxy list)
  implies = impliesRecord  (RLProxy :: RLProxy list)
  not = notRecord  (RLProxy :: RLProxy list)

foreign import boolConj :: Boolean -> Boolean -> Boolean
foreign import boolDisj :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean

-- | A class for records where all fields have `HeytingAlgebra` instances, used
-- | to implement the `HeytingAlgebra` instance for records.
class HeytingAlgebraRecord rowlist row subrow | rowlist -> subrow where
  ffRecord :: RLProxy rowlist -> RProxy row -> Record subrow
  ttRecord :: RLProxy rowlist -> RProxy row -> Record subrow
  impliesRecord :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  disjRecord :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  conjRecord :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  notRecord :: RLProxy rowlist -> Record row -> Record subrow

instance heytingAlgebraRecordNil :: HeytingAlgebraRecord RL.Nil row () where
  conjRecord _ _ _ = {}
  disjRecord _ _ _ = {}
  ffRecord _ _ = {}
  impliesRecord _ _ _ = {}
  notRecord _ _ = {}
  ttRecord _ _ = {}

instance heytingAlgebraRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , HeytingAlgebraRecord rowlistTail row subrowTail
       , HeytingAlgebra focus
       )
    => HeytingAlgebraRecord (RL.Cons key focus rowlistTail) row subrow where
  conjRecord _ ra rb = insert (conj (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = conjRecord (RLProxy :: RLProxy rowlistTail) ra rb

  disjRecord _ ra rb = insert (disj (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = disjRecord (RLProxy :: RLProxy rowlistTail) ra rb

  impliesRecord _ ra rb = insert (implies (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = impliesRecord (RLProxy :: RLProxy rowlistTail) ra rb

  ffRecord _ row = insert ff tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = ffRecord (RLProxy :: RLProxy rowlistTail) row

  notRecord _ row
    = insert (not (get row)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = notRecord (RLProxy :: RLProxy rowlistTail) row

  ttRecord _ row = insert tt tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = ttRecord (RLProxy :: RLProxy rowlistTail) row
