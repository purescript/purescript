module Main where

type A a = Array a

data Foo a = Foo (A a) | Bar

foo (Foo []) = Bar

main = Debug.Trace.trace "Done"
