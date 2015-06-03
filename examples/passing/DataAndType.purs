module Main where

import Prelude

data A = A B

type B = A

main = Debug.Trace.trace "Done"
