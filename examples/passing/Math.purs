module Main where

  import Prelude
  import Trace
  import Eff

  main = do
    print $ Math.sin 0
    print $ Math.sin $ Math.pi * 0.5
    print $ Math.e / Math.ln2