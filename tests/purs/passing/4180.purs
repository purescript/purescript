module Main where

import Effect.Console (log)

class C (t :: Type)
instance C (f a)

f :: C (Array String) => Int
f = 0

v :: Int
v = f

main = log "Done"
