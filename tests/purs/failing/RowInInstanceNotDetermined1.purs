-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

-- `c` not mentioned in any fundeps
class C a b c | a -> b
instance c :: C Unit Unit {}

