module Conditional where

  fns = \f -> if f true then f else \x -> x 

  bin = \x -> if x && !x then x else x

  not = \x -> if x then false else true
    
module Main where

main = Trace.trace "Done"
