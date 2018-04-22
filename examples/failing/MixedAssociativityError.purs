-- @shouldFailWith MixedAssociativityError
module Main where

import Prelude

infix 6 add as .+
infixr 6 add as +.

n = 1 .+ 2 * 3 +. 6 + 7
