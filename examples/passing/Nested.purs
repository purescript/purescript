module Main where

import Prelude
import Effect.Console (log)

data Extend r a = Extend { prev :: r a, next :: a }

data Matrix r a = Square (r (r a)) | Bigger (Matrix (Extend r) a)

main = log "Done"
