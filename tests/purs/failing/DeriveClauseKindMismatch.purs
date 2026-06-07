-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

data Box a = Box a
  derive (Eq)
