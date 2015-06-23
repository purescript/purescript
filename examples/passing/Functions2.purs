module Main where

import Prelude
import Assert

test :: forall a b. a -> b -> a
test = \const _ -> const

main = do
  let value = test "Done" {}
  if value == "Done"
    then Debug.Trace.trace "Done"
    else error "Not done"
