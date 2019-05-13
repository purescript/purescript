-- @shouldFailWith CycleInDictDeclaration
module Main where

class B0

instance b0 :: B0

class B1 a where
  x :: a

instance b1Int :: B1 Int where
  x = 0

class C a where
  c0 :: a
  c1 :: a

instance cInt :: (B0, B1 Int) => C Int where
  c0 = 0
  c1 =
    let
      const' :: forall a. B1 Int => a -> Int -> a
      const' a _ = a
    in
      const' c0 x
