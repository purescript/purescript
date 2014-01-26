module Nested where

  data Extend r a = Extend { prev :: r a, next :: a }

  data Matrix r a = Square (r (r a)) | Bigger (Matrix (Extend r) a)
    
module Main where

main = Trace.trace "Done"
