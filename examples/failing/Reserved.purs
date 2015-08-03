-- @shouldFailWith ErrorParsingModule
module Main where

import Prelude

(<) :: Number -> Number -> Number
(<) a b = !(a >= b)
