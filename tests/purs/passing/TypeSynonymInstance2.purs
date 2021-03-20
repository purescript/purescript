module Main where

import Effect.Console (log)

data D
type S = D

class C0 a
class C0 a <= C1 a

instance c0 :: C0 D
instance c1 :: C1 S

main = log "Done"
