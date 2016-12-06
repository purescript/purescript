-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

-- no fundeps
class C a b
instance c :: C Unit {}

