-- @shouldFailWith NoInstanceFound
module Main where

class Multi a b c d e f | f e -> a b c d, a b -> c d e f where
  useMulti :: Int

multi :: Int
multi = useMulti
