module Main where

import Prelude
import Effect.Console (log)
import Prim.Int (class ToString)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

type One = 1
type NegOne = (-1)
type Zero = 0

testToString :: forall i s. ToString i s => Proxy i -> Proxy s
testToString _ = Proxy

posToString :: Proxy "1"
posToString = testToString (Proxy :: Proxy 1)

negToString :: Proxy "-1"
negToString = testToString (Proxy :: Proxy (-1))

zeroToString :: Proxy "0"
zeroToString = testToString (Proxy :: Proxy 0)

posToStringTA :: Proxy "1"
posToStringTA = testToString (Proxy :: Proxy One)

negToStringTA :: Proxy "-1"
negToStringTA = testToString (Proxy :: Proxy NegOne)

zeroToStringTA :: Proxy "0"
zeroToStringTA = testToString (Proxy :: Proxy Zero)

main = log "Done"