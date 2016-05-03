module Main where

import Prelude
import Control.Monad.Eff.Console (log)

f1 :: (_ -> _) -> _
f1 g = g 1

f2 :: _ -> _
f2 _ = "Done"

main = log $ f1 f2
