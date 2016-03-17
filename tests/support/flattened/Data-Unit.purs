module Data.Unit where

import Data.Show (class Show)

-- | The `Unit` type has a single inhabitant, called `unit`. It represents
-- | values with no computational content.
-- |
-- | `Unit` is often used, wrapped in a monadic type constructor, as the
-- | return type of a computation where only
-- | the _effects_ are important.
foreign import data Unit :: *

-- | `unit` is the sole inhabitant of the `Unit` type.
foreign import unit :: Unit

instance showUnit :: Show Unit where
  show _ = "unit"
