module Main where

  data Extend r a = Extend { prev :: r a, next :: a }

  data Matrix r a = Square (r (r a)) | Bigger (Matrix (Extend r) a)

  main = Debug.Trace.trace "Done"
