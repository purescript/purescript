-- @shouldWarnWith MissingSuperclassInstance
module Main where

import Prelude

newtype X = X String

derive via String instance ordX :: Ord X
