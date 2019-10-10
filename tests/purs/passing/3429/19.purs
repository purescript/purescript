-- Cf. 20.purs and failing/3429-20.purs
module Mod19 where

class B

instance b :: B

class C0 a where
  c0 :: B => a

class C1 a where
  c1 :: B => a

instance c0Int :: C0 Int where
  c0 = c1

instance c1Int :: C1 Int where
  c1 = c0

-- The following two values induce endless looping.
-- d0 :: Int
-- d0 = c0
--
-- d1 :: Int
-- d1 = c1
