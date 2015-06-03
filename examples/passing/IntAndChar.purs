module Main where

import Prelude
import Control.Monad.Eff
import Assert

f 1 = 1
f _ = 0

g 'a' = 'a'
g _ = 'b'

main = do
  assert $ f 1 == 1
  assert $ f 0 == 0
  assert $ g 'a' == 'a'
  assert $ g 'b' == 'b'
  Debug.Trace.trace "Done"
