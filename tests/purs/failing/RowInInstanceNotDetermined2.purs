-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

-- `b` isn't determined by anything that `b` doesn't determine
class C a b | a -> b, b -> a
instance c :: C Unit {}

