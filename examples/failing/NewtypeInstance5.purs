-- @shouldFailWith InvalidNewtypeInstance
module Main where

import Prelude

newtype X a = X a

derive newtype instance functorX :: Functor X
