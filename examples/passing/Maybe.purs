module Main where

import Prelude
import Data.Maybe
import Global

main = do
  let test1 = fromMaybe 0 $ do
    a <- Just 1
    b <- Just 2
    return (a + b)
  Debug.Trace.print test1
