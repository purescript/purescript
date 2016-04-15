-- @shouldFailWith CannotGeneralizeRecursiveFunction
module Main where

import Prelude

test n m | n <= 1 = m
         | otherwise = test (n - 1) (m <> m)

