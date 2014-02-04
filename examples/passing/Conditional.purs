module Conditional where

  fns = \f -> if f true then f else \x -> x 

  not = \x -> if x then false else true
    
module Main where

main = Trace.trace "Done"
