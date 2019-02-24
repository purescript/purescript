-- @shouldFailWith OverlappingInstances
module Main where

import Prelude

class Convert a b | a -> b where
  convert :: a -> b

type Bar = String

instance convertSB :: Convert String Bar where
  convert s = s

instance convertSS :: Convert String String where
  convert s = s
