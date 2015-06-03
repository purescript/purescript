module Main where

import Prelude

test = ((\x -> x+1.0) >>> (\x -> x*2.0)) 4.0

main = Debug.Trace.trace "Done"
