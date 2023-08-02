-- @shouldFailWith NoInstanceFound
module Main where

class Single tyNotAppearInBody where
  useSingle :: Int

single :: Int
single = useSingle
