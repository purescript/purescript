module Main where

import Prelude
import Control.Monad.Eff.Console (print)

main = do
  print (test 3 [1, 2])

test n m | n <= 1 = m
         | otherwise = test (n - 1) (m <> m)

