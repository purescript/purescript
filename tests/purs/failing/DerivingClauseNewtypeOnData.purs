-- @shouldFailWith InvalidNewtypeInstance
module DerivingClauseNewtypeOnData where

import Prelude

data Pair = Pair Int Int
  derive newtype (Eq)
