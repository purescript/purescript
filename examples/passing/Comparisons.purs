module Main where

import Prelude
import Control.Monad.Eff
import Debug.Trace
import Assert

main = do
  assert (1.0 < 2.0)
  assert (2.0 == 2.0)
  assert (3.0 > 1.0)
  assert ("a" < "b")
  assert ("a" == "a")
  assert ("z" > "a")
  trace "Done!"
