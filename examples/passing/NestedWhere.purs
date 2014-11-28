module Main where

f x = g x
  where
  g x = go x
    where
    go x = go1 (x - 1)
    go1 x = go x

main = Debug.Trace.trace "Done"
