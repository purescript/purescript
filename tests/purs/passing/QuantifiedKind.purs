module Main where

import Effect.Console (log)

data Proxy a = Proxy

test :: forall k (a :: k). Proxy a
test = Proxy

main = log "Done"
