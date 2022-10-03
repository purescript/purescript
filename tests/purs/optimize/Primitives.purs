-- This test checks that no unused Semiring abstractions are introduced when
-- the operators are compiled to JS primitives.

module Main where

import Prelude

f :: Int -> Int -> Int
f x y = x * (y + 1)
