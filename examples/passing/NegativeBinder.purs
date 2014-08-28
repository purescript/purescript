module Main where

test :: Number -> Boolean
test -1 = false
test _  = true

main = Debug.Trace.trace "Done"
