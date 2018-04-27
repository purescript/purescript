-- @shouldFailWith CycleInDeclaration
module Main where

import Prelude

x = x
