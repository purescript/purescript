module Main where

test = ((\x -> x+1) >>> (\x -> x*2)) 4

main = Debug.Trace.trace "Done"
