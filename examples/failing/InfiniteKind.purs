-- @shouldFailWith InfiniteKind 

module Main where

data F a = F (a a)
