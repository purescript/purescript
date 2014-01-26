module OptimizerBug where

x = 1 + y

y = x
    
module Main where

main = Trace.trace "Done"
