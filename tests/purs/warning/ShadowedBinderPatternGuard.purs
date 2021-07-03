-- @shouldWarnWith ShadowedName
module Main where

f :: Int -> Int
f _ | i <- true -- this i is shadowed
    , i <- 1234
    = i
