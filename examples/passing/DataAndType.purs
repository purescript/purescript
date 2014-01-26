module DataAndType where

data A = A B

type B = A
    
module Main where

main = Trace.trace "Done"
