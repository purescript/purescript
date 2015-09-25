module Main where

import Prelude

f x y z =
  let f 1.0 2.0 3.0 = 1.0
  in f x z y

main = Control.Monad.Eff.Console.log $ show $ f 1.0 3.0 2.0
