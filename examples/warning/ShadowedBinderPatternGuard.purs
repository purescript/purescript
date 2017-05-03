-- @shouldWarnWith ShadowedName
module Main where

f :: Int -> Int
f n | i <- true -- this i is shadowed
	, i <- 1234
	= i