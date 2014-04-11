module Main where

data E a b = L a | R b

lefts :: forall a b. [E a b] -> [a]
lefts = go []
  where
  go :: forall a b. [a] -> [E a b] -> [a]
  go ls [] = ls
  go ls (L a : rest) = go (a : ls) rest
  go ls (_ : rest) = go ls rest

main = Debug.Trace.trace "Done"
