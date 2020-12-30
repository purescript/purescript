module Main where

import Effect.Console (log)

data D
type S = D
newtype N a = N a

class C a b

derive newtype instance c :: C S a => C S (N a)

main = log "Done"
