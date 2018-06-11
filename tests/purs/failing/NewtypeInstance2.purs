-- @shouldFailWith InvalidNewtypeInstance
module Main where

import Prelude

data X a = X a a

derive newtype instance showX :: Show a => Show (X a)
