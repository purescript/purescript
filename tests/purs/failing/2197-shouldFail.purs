-- @shouldFailWith ScopeConflict
module Main where

import Prim as P
import Prim (Number)

type Number = P.Number

z :: Number
z = 0.0
