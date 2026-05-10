-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

data Either2 a b = Left2 a | Right2 b
  derive (Eq)
