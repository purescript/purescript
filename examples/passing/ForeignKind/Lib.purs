module ForeignKinds.Lib (kind Nat, Kinded, Zero, Succ, N0, N1, N2, N3, NatProxy(..), class AddNat, addNat, proxy1, proxy2) where

-- declaration

foreign import kind Nat

-- use in foreign data

foreign import data Zero :: Nat
foreign import data Succ :: Nat -> Nat

-- use in data

data NatProxy (t :: Nat) = NatProxy

-- use in type sig

succProxy :: forall n. NatProxy n -> NatProxy (Succ n)
succProxy _ = NatProxy

-- use in alias

type Kinded f = f :: Nat

type KindedZero = Kinded Zero

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2

-- use of alias

proxy0 :: NatProxy N0
proxy0 = NatProxy

proxy1 :: NatProxy N1
proxy1 = NatProxy

proxy2 :: NatProxy N2
proxy2 = NatProxy

proxy3 :: NatProxy N3
proxy3 = NatProxy

-- use in class

class AddNat (l :: Nat) (r :: Nat) (o :: Nat) | l -> r o

instance addNatZero
  :: AddNat Zero r r

instance addNatSucc
  :: AddNat l r o
  => AddNat (Succ l) r (Succ o)

-- use of class

addNat :: forall l r o. AddNat l r o => NatProxy l -> NatProxy r -> NatProxy o
addNat _ _ = NatProxy
