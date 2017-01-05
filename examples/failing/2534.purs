-- @shouldFailWith InfiniteType
module Main where

foo :: Array Int -> Int
foo xs = go xs where
  go :: Array _ -> Int
  go [] = 0
  go xs = go [xs]
