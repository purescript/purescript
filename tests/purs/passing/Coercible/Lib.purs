module Coercible.Lib
  ( module Coercible.Lib2
  , NTLib1 (..)
  , NTLib3 (..)
  ) where

import Coercible.Lib2

newtype NTLib1 a = NTLib1 a

newtype NTLib3 a b = NTLib3 a
type role NTLib3 representational representational
