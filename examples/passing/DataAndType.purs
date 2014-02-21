module DataAndType where

data A = A B

type B = A
    
module Main where

main = Debug.Trace.trace "Done"
