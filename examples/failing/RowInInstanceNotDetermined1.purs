-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

-- `c` not mentioned in any fundeps
class C1 a b c | a -> b, b -> a
instance c1 :: C1 Unit Unit {}

