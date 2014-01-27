module Main where

import Prelude
import Eff
import Arrays
import Global

main = do
  let test1 = concat [1, 2, 3] [4, 5, 6]
  Trace.print test1
