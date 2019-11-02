-- @shouldFailWith CycleInTypeSynonym
module Main where

import Prelude

type T = T
