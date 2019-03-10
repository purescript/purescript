module Data.BooleanAlgebra
  ( class BooleanAlgebra
  , module Data.HeytingAlgebra
  , class BooleanAlgebraRecord
  ) where

import Data.HeytingAlgebra (class HeytingAlgebra, class HeytingAlgebraRecord, ff, tt, implies, conj, disj, not, (&&), (||))
import Data.Symbol (class IsSymbol)
import Data.Unit (Unit)
import Prim.Row as Row
import Prim.RowList as RL

-- | The `BooleanAlgebra` type class represents types that behave like boolean
-- | values.
-- |
-- | Instances should satisfy the following laws in addition to the
-- | `HeytingAlgebra` law:
-- |
-- | - Excluded middle:
-- |   - `a || not a = tt`
class HeytingAlgebra a <= BooleanAlgebra a

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean
instance booleanAlgebraUnit :: BooleanAlgebra Unit
instance booleanAlgebraFn :: BooleanAlgebra b => BooleanAlgebra (a -> b)
instance booleanAlgebraRecord :: (RL.RowToList row list, BooleanAlgebraRecord list row row) => BooleanAlgebra (Record row)

-- | A class for records where all fields have `BooleanAlgebra` instances, used
-- | to implement the `BooleanAlgebra` instance for records.
class HeytingAlgebraRecord rowlist row subrow <= BooleanAlgebraRecord rowlist row subrow | rowlist -> subrow

instance booleanAlgebraRecordNil :: BooleanAlgebraRecord RL.Nil row ()

instance booleanAlgebraRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , BooleanAlgebraRecord rowlistTail row subrowTail
       , BooleanAlgebra focus
       )
    => BooleanAlgebraRecord (RL.Cons key focus rowlistTail) row subrow
