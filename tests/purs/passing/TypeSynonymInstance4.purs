module Main where

import Effect.Console (log)

data D
type S = D
newtype N a = N a

class C a

derive newtype instance c :: C S => C (N S)

main = log "Done"
