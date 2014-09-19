module Main where

data Foo x = Foo x 

test x@(Foo x) = x