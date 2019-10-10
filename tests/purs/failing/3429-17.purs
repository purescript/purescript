-- @shouldFailWith CycleInDictDeclaration
module Main where

import Data.Tuple (Tuple(Tuple))

class B0

instance b0 :: B0

class B1 a where
  x1 :: a

instance b1Int :: B1 Int where
  x1 = 0

class B2 a where
  x2 :: a

instance b2Tuple :: B1 a => B2 (Tuple a a) where
  x2 = Tuple x1 x1

class B1 a <= B3 a where
  x3 :: a

instance b3Int :: B3 Int where
  x3 = x1

class C a where
  c0 :: a
  c1 :: a

instance cInt
  :: ( B0
     , B1 Int
     , B2 (Tuple Int Int)
     , B3 Int
     )
  => C Int where
  c0 = 0
  c1 =
    let
      const' :: forall a b c d. a -> b -> c -> d -> a
      const' a _ _ _ = a
    in
      const' c0 (x1 :: Int) (x2 :: Tuple Int Int) (x3 :: Int)
