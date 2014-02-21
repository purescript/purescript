module Main where

import Prelude
import Data.Array

tails = map (\(_:xs) -> xs)

main = 
  let ts = tails [[1, 2, 3], [4, 5], [6]] in
  if ts == [[2, 3], [5], []] 
  then Debug.Trace.trace "Done"
  else Control.Monad.Error.throwError "Incorrect result from 'tails'."
