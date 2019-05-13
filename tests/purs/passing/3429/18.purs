-- Cf. 17.purs
module Mod18 where

class B

instance b :: B

class C a where
  c0 :: B => a
  c1 :: B => a

instance cInt :: C Int where
  c0 = c0
  c1 = c1

-- The following two values induce endless looping.
-- d0 :: Int
-- d0 = c0
--
-- d1 :: Int
-- d1 = c1
