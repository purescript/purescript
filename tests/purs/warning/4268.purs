module Main where

f :: Partial => Int -> Int
f 0 = f 1
