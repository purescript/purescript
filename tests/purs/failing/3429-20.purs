-- @shouldFailWith CycleInDictDeclaration
-- @shouldFailWith CycleInDictDeclaration
-- Cf. 3429-20.purs, passing/3429/19.purs, passing/3429/20.purs, passing/3429/21.purs
module Main where

class C0 a where
  c0 :: Int -> a

class C1 a where
  c1 :: Int -> a

instance c0Int :: C0 Int where
  c0 = c1

instance c1Int :: C1 Int where
  c1 = c0
