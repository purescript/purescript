-- @shouldFailWith NoInstanceFound
module Main where

f :: Int -> Int
f x | 1 <- x = x