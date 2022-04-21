module Main where

import Prelude
import Effect.Console (log)
import Prim.Int (class Add, class Mul, class ToString)

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

intAdd
  :: forall i1 i2 i3
   . Add i1 i2 i3
  => Proxy i1
  -> Proxy i2
  -> Proxy i3
intAdd _ _ = Proxy

intMul
  :: forall i1 i2 i3
   . Mul i1 i2 i3
  => Proxy i1
  -> Proxy i2
  -> Proxy i3
intMul _ _ = Proxy

testAdd :: Proxy "4"
testAdd = testToString (intAdd (Proxy :: _ 1) (Proxy :: _ 3))

testMul :: Proxy "6"
testMul = testToString (intMul (Proxy :: _ 2) (Proxy :: _ 3))

testMulAdd :: Proxy "10"
testMulAdd = testToString (intAdd (Proxy :: _ 4) (intMul (Proxy :: _ 2) (Proxy :: _ 3)))

testAddMul :: Proxy "20"
testAddMul = testToString (intMul (Proxy :: _ 4) (intAdd (Proxy :: _ 2) (Proxy :: _ 3)))

_maxInt = Proxy :: _ 2147483647

testMax :: Proxy "2147483647"
testMax = testToString _maxInt

testBeyondMax :: Proxy "4294967294"
testBeyondMax = testToString (intMul _maxInt (Proxy :: _ 2))

main = log "Done"
