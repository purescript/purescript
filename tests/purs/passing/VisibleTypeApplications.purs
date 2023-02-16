module Main where

import Prelude
import Effect.Console (log)

foreign import data Id :: forall (a :: Type). a -> a

identityCheck :: forall (@f :: forall (a :: Type). a -> a). Int
identityCheck = 0

identityPass :: Int
identityPass = identityCheck @Id

foreign import data Const :: forall a b. a -> b -> a

constCheck :: forall (a :: Type) (@f :: forall (b :: Type). b -> a). Int
constCheck = 0

constPass :: Int
constPass = constCheck @(Const Int)

-- Type variables in class heads and data declarations are always visible.

class ConstClass a where
  constClass :: forall @b. a -> b -> a
    
instance ConstClass a where
  constClass a _ = a

constClassInt = constClass @Int @Number

data Tree a = Leaf a | Branch (Tree a) (Tree a)

treeInt :: Int -> Tree Int
treeInt = Leaf @Int

treeInt' :: Tree Int -> Tree Int -> Tree Int
treeInt' = Branch @Int

main = log "Done"
