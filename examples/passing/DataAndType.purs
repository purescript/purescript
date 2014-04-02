module Main where

data A = A B

type B = A

main = Debug.Trace.trace "Done"
