-- Taken from https://github.com/LiamGoodacre/purescript-fun-with-fundeps

module Main where

import Control.Monad.Eff.Console (log)

-- Nat : Type
data Z
data S n

type S2 n = S (S n)
type S3 n = S (S2 n)
type S4 n = S (S3 n)
type S5 n = S (S4 n)
type S15 n = S5 (S5 (S5 n))

class NatPlus l r o | l r -> o
instance natPlusZ :: NatPlus Z r r
instance natPlusS :: (NatPlus l r o) => NatPlus (S l) r (S o)

class NatMult l r o | l r -> o
instance natMultZ :: NatMult Z n Z
instance natMultS :: (NatMult m n r, NatPlus n r s) => NatMult (S m) n s

-- Foreign Vect
foreign import data FVect :: * -> * -> *
foreign import fnil :: forall e. FVect Z e
foreign import fcons :: forall n e. e -> FVect n e -> FVect (S n) e
foreign import fappend :: forall l r o e. (NatPlus l r o) => FVect l e -> FVect r e -> FVect o e
foreign import fflatten :: forall f s t o. (NatMult f s o) => FVect f (FVect s t) -> FVect o t
foreign import ftoArray :: forall n e. FVect n e -> Array e

-- should be able to figure these out
fsingleton x = fcons x fnil
fexample = fcons 1 (fsingleton 2) `fappend` fsingleton 3 `fappend` fcons 4 (fsingleton 5)
fexample2 = fexample `fappend` fexample `fappend` fexample
fexample3 = fsingleton fexample `fappend` fsingleton fexample `fappend` fsingleton fexample

fexample4 = fflatten fexample3
 
main = log "Done"
