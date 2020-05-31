module Main where

import Effect.Console (log)

data Proxy a = Proxy

data X a = X (Y a => Proxy a)

class Z (X a) <= Y a

class Z a

test1 = X (Proxy :: _ Int)
test2 = X (Proxy :: _ "foo")

main = log "Done"
