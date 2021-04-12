-- @shouldWarnWith ShadowedName
module Main where

f :: Int -> Int -> Int
f n = 
  let _ = n in
  \(n) -> n
