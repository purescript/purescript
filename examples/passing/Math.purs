module Main where

  import Prelude
  import Debug.Trace
  import Control.Monad.Eff

  main = do
    print $ Math.sin 0
    print $ Math.sin $ Math.pi * 0.5
    print $ Math.e / Math.ln2