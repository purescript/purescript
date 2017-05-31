-- @shouldWarnWith MissingNewtypeSuperclassInstance
module Main where

import Prelude

newtype X = X String

derive newtype instance ordX :: Ord X
