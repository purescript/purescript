module Main where

import Effect.Console (log)
import Prim.Nat (class Compare)
import Prim.Ordering (EQ, GT, LT)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

assertLesser :: forall l r. Compare l r LT => Proxy ( left :: l, right :: r )
assertLesser = Proxy

assertGreater :: forall l r. Compare l r GT => Proxy ( left :: l, right :: r )
assertGreater = Proxy

assertEqual :: forall l r. Compare l r EQ => Proxy ( left :: l, right :: r )
assertEqual = Proxy

symmLt :: forall m n. Compare m n GT => Proxy ( left :: n, right :: m )
symmLt = assertLesser

symmGt :: forall m n. Compare m n LT => Proxy ( left :: n, right :: m )
symmGt = assertGreater

symmEq :: forall m n. Compare m n EQ => Proxy ( left :: n, right :: m )
symmEq = assertEqual

reflEq :: forall (n :: Nat). Proxy ( left :: n, right :: n )
reflEq = assertEqual

transLt :: forall m n p. Compare m n LT => Compare n p LT => Proxy n -> Proxy ( left :: m, right :: p )
transLt _ = assertLesser

transLtEq :: forall m n p. Compare m n LT => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transLtEq _ = assertLesser

transEqLt :: forall m n p. Compare m n EQ => Compare n p LT => Proxy n -> Proxy ( left :: m, right :: p )
transEqLt _ = assertLesser

transGt :: forall m n p. Compare m n GT => Compare n p GT => Proxy n -> Proxy ( left :: m, right :: p )
transGt _ = assertGreater

transGtEq :: forall m n p. Compare m n GT => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transGtEq _ = assertGreater

transEqGt :: forall m n p. Compare m n EQ => Compare n p GT => Proxy n -> Proxy ( left :: m, right :: p )
transEqGt _ = assertGreater

transEq :: forall m n p. Compare m n EQ => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transEq _ = assertEqual

transSymmLt :: forall m n p. Compare n m GT => Compare n p LT => Proxy n -> Proxy ( left :: m, right :: p )
transSymmLt _ = assertLesser

transSymmLtEq :: forall m n p. Compare n m GT => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transSymmLtEq _ = assertLesser

transSymmEqLt :: forall m n p. Compare n m EQ => Compare n p LT => Proxy n -> Proxy ( left :: m, right :: p )
transSymmEqLt _ = assertLesser

transSymmGt :: forall m n p. Compare n m LT => Compare n p GT => Proxy n -> Proxy ( left :: m, right :: p )
transSymmGt _ = assertGreater

transSymmGtEq :: forall m n p. Compare n m LT => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transSymmGtEq _ = assertGreater

transSymmEqGt :: forall m n p. Compare n m EQ => Compare n p GT => Proxy n -> Proxy ( left :: m, right :: p )
transSymmEqGt _ = assertGreater

transSymmEq :: forall m n p. Compare n m EQ => Compare n p EQ => Proxy n -> Proxy ( left :: m, right :: p )
transSymmEq _ = assertEqual

litLt :: Proxy ( left :: 0, right :: 1 )
litLt = assertLesser

litGt :: Proxy ( left :: 1, right :: 0 )
litGt = assertGreater

litEq :: Proxy ( left :: 0, right :: 0 )
litEq = assertEqual

main = log "Done"
