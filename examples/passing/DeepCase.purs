module Main where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Control.Monad.ST

f x y =
  let
    g = case y of
	  0.0 -> x
          x -> 1.0 + x * x
  in g + x + y

main = print $ f 1.0 10.0
