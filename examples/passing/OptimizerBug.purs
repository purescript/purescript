module Main where

import Prelude

x a = 1 + y a

y a = x a

main = Debug.Trace.trace "Done"
