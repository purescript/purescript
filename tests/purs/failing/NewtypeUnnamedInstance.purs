-- @shouldFailWith InvalidNewtypeInstance
module Main where

import Prelude

data X = X

derive newtype instance Show X
