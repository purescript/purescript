module Main where

import Prelude
import Maybe

main = do
  let test1 = fromMaybe $ do
    a <- Just 1
    b <- Just 2
    ret (a + b)
  Trace.trace "Done"
