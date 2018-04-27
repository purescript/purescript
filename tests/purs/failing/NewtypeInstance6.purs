-- @shouldFailWith InvalidNewtypeInstance
module Main where

import Prelude

newtype X a b = X (Array b)

derive newtype instance functorX :: Functor X
