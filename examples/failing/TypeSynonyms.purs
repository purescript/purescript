-- @shouldFailWith CycleInTypeSynonym
module Main where

import Prelude

type T1 = Array T2

type T2 = T1
