module OneConstructor where

data One a = One a

one (One a) = a
    
module Main where

main = Debug.Trace.trace "Done"
