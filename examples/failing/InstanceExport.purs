-- @shouldFailWith TransitiveExportError
module InstanceExport (S(..), f) where

import Prelude

newtype S = S String

class F a where
  f :: a -> String

instance fs :: F S where
  f (S s) = s

module Test where

import InstanceExport
import Prelude

test = f $ S "Test"
