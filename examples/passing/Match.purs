module Match where

  data Foo a = Foo

  foo = \f -> case f of Foo -> "foo"
    
module Main where

main = Trace.trace "Done"
