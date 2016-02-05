-- @shouldFailWith IncorrectAnonymousArgument
module Main where

import Prelude

test :: Int -> Int
test = 1 + 2 * _
