module Main where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.ST

f x y =
  let
    g = case y of
	  0 -> x
          x -> 1 + x * x
  in g + x + y

main = print $ f 1 10
