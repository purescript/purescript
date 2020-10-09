module UnsafeCoerce where

import Data.Unit (Unit)

newtype UnsafeCoerce a = UnsafeCoerce Unit

type role UnsafeCoerce representational
