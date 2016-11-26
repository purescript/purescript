-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

-- no fundeps
class C0 a b c
instance c0 :: C0 Unit Unit {}

