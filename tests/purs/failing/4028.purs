-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

import Type.Proxy (Proxy(..))

foreign import data Peano :: Type

foreign import data Z :: Peano
foreign import data S :: Peano -> Peano

class TLShow :: forall k. k -> Constraint
class TLShow i where
  tlShow :: Proxy i -> String

instance tlShow2 :: TLShow (S (S Z)) where
  tlShow _ = "2"
else instance tlShow0 :: TLShow Z where
  tlShow _ = "0"
else instance tlShowS :: TLShow x => TLShow (S x) where
  tlShow _ = "S" <> tlShow (Proxy :: Proxy x)

peano :: Int -> String
peano = go (Proxy :: Proxy Z)
  where
  go :: forall i. TLShow i => Proxy i -> Int -> String
  go p 0 = tlShow p
  go _ n = go (Proxy :: Proxy (S i)) (n - 1)
