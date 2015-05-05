module Main where

import Prelude
import Debug.Trace

f1 :: (_ -> _) -> _
f1 g = g 1

f2 :: _ -> _
f2 _ = "Done"

main = Debug.Trace.trace $ f1 f2

