module InstanceExport (S(..), f) where

import Prelude

newtype S = S String

class F a where
  f :: a -> String

instance fs :: F S where
  f (S s) = s
