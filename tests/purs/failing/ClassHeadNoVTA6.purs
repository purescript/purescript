-- @shouldFailWith NoInstanceFound
module Main where

-- Verify that the order of fun deps declared here
-- match the order shown in output
class Multi a b c d e f | f e -> a b c d, a b -> c d e f where
  useMulti :: Int

multi :: Int
multi = useMulti
