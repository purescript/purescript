module NamedPatterns where

  foo = \x -> case x of 
    y@{ foo = "Foo" } -> y
    y -> y
    
module Main where

main = Trace.trace "Done"
