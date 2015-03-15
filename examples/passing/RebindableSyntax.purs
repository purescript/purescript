module Main where

example :: String
example = do
  "Do"
  " notation"
  " for"
  " monoids"
  where
  (>>=) x f = x <> f unit

main = Debug.Trace.trace example
