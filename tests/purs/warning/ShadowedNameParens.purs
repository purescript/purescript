-- @shouldWarnWith ShadowedName
module Main where

f :: Int -> Int -> Int
f n = \(n) -> 1
