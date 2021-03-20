module Foo where

import Prelude

tie :: Int -> Int -> Int
tie a b = (a - 1) * (b + 1)

infix 5 tie as -#-
