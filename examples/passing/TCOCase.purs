module Main where

import Prelude

data Data = One | More Data

main = Debug.Trace.trace (from (to 10000.0 One))
  where
  to 0.0 a = a
  to n a = to (n - 1.0) (More a)
  from One = "Done"
  from (More d) = from d
