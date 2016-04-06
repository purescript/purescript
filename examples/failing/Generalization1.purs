-- @shouldFailWith CannotGeneralizeRecursiveFunction
module Main where

import Prelude

foo 0 x _ = x
foo n x y = x <> bar (n - 1) x y

bar 0 x _ = x
bar n x y = y <> foo (n - 1) x y

