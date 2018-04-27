-- @shouldFailWith CycleInDeclaration
module Main where

import Prelude

test = let x = x in x
