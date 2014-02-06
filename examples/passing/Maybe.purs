module Main where

import Prelude
import Maybe
import Global

main = do
  let test1 = fromMaybe 0 $ do
    a <- Just 1
    b <- Just 2
    return (a + b)
  Trace.print test1
