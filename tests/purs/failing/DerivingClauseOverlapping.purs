-- @shouldFailWith OverlappingInstances
module Main where

import Prelude

data Color = Red | Green | Blue
  derive (Show)

derive instance Show Color
