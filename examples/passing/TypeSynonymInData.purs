module Main where

type A a = [a]

data Foo a = Foo (A a) | Bar

foo (Foo []) = Bar

main = Debug.Trace.trace "Done"
