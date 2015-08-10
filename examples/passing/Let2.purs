module Main where

import Prelude

test =
  let f :: Number -> Boolean
      f 0.0 = false
      f n = g (n - 1.0)

      g :: Number -> Boolean
      g 0.0 = true
      g n = f (n - 1.0)

      x = f 1.0
  in not x

main = Control.Monad.Eff.Console.print test
