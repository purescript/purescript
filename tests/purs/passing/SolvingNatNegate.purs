module Main where

import Effect.Console (log)
import Prim.Nat (class Add, class Compare, class Negate)
import Prim.Ordering (EQ)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

compareNat :: forall l r o. Compare l r o => Proxy l -> Proxy r -> Proxy o
compareNat _ _ = Proxy

a :: forall n. Add 200 n 100 => Proxy n
a = Proxy

b :: forall n. Negate 100 n => Proxy n
b = Proxy

c :: forall n. Negate n 100 => Proxy n
c = Proxy

test_ab :: Proxy EQ
test_ab = compareNat a b

test_ac :: Proxy EQ
test_ac = compareNat a c

main = log "Done"
