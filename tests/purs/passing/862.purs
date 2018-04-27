module Main where

import Prelude
import Effect.Console

id' = (\x -> x) <$> \y -> y

main = log (id' "Done")
