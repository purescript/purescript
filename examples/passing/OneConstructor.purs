module Main where

import Prelude

data One a = One a

one' (One a) = a

main = Debug.Trace.trace "Done"
