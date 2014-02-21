module Main where

import Prelude
import Arrays

tails = map (\(_:xs) -> xs)

main = 
  let ts = tails [[1, 2, 3], [4, 5], [6]] in
  if ts == [[2, 3], [5], []] 
  then Trace.trace "Done"
  else Errors.throwError "Incorrect result from 'tails'."
