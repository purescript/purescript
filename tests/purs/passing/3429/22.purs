-- Cf. failing/3429-22.purs
module Mod22 where

class B

instance b :: B

class C a where
  c :: B => a

instance cInt :: C Int where
  c = c

-- The following value induces endless looping.
-- c0 :: Int
-- c0 = c
