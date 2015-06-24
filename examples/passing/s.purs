module Main where

import Prelude

s = \x y z -> x z (y z)

main = Debug.Trace.trace "Done"
