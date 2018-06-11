-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

main = do
  (true `not` _)
