module Main where

import Prelude
import Assert

mkValue :: Number -> Number
mkValue id = id

main = do
  let value = mkValue 1.0
  if value == 1.0
    then Debug.Trace.trace "Done"
    else error "Not done"
