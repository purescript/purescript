-- @shouldFailWith NoInstanceFound
module Main where

class Multi a b c d e f | a c -> d f, b c -> a d where
  useMulti :: Int

multi :: Int
multi = useMulti
