module Data.Show
  ( class Show, show
  , class ShowRecordFields, showRecordFields
  ) where

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Data.RowList (RLProxy(..))

-- | The `Show` type class represents those types which can be converted into
-- | a human-readable `String` representation.
-- |
-- | While not required, it is recommended that for any expression `x`, the
-- | string `show x` be executable PureScript code which evaluates to the same
-- | value as the expression `x`.
class Show a where
  show :: a -> String

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showInt :: Show Int where
  show = showIntImpl

instance showNumber :: Show Number where
  show = showNumberImpl

instance showChar :: Show Char where
  show = showCharImpl

instance showString :: Show String where
  show = showStringImpl

instance showArray :: Show a => Show (Array a) where
  show = showArrayImpl show

instance showRecord :: (RL.RowToList rs ls, ShowRecordFields ls rs) => Show (Record rs) where
  show record = case showRecordFields (RLProxy :: RLProxy ls) record of
    [] -> "{}"
    fields -> join " " ["{", join ", " fields, "}"]

-- | A class for records where all fields have `Show` instances, used to
-- | implement the `Show` instance for records.
class ShowRecordFields rowlist row where
  showRecordFields :: RLProxy rowlist -> Record row -> Array String

instance showRecordFieldsNil :: ShowRecordFields RL.Nil row where
  showRecordFields _ _ = []

instance showRecordFieldsCons
    :: ( IsSymbol key
       , ShowRecordFields rowlistTail row
       , Show focus
       )
    => ShowRecordFields (RL.Cons key focus rowlistTail) row where
  showRecordFields _ record
    = cons (join ": " [ key, show focus ]) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      focus = unsafeGet key record :: focus
      tail = showRecordFields (RLProxy :: RLProxy rowlistTail) record

foreign import showIntImpl :: Int -> String
foreign import showNumberImpl :: Number -> String
foreign import showCharImpl :: Char -> String
foreign import showStringImpl :: String -> String
foreign import showArrayImpl :: forall a. (a -> String) -> Array a -> String
foreign import cons :: forall a. a -> Array a -> Array a
foreign import join :: String -> Array String -> String
