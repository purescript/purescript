-- @shouldFailWith InvalidNewtypeInstance
module Main where

import Prelude

data X = X | Y

derive newtype instance showX :: Show X
