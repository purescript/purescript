-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

-- `c` appears in determiner position
class C2 a b c | a -> c, c -> b
instance c2 :: C2 Unit Unit {}

