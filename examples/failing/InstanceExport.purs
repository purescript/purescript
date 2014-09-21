module InstanceExport (S(..), f) where

newtype S = S String

class F a where
  f :: a -> String

instance fs :: F S where
  f (S s) = s

module Test where

import InstanceExport

test = f $ S "Test"
