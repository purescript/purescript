module M1 (x) where

  x = 1
  y = 0

module Main where

  import M1
  
  -- should fail as y is not exported from M1
  testY = y

  main = Debug.Trace.trace "Done"
