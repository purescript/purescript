-- @shouldFailWith InvalidCoercibleInstanceDeclaration
module Main where

import Prim.Coerce (class Coercible)

data D

instance coercible :: Coercible D D
