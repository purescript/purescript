module Main where

  import Prelude
  import Data.Foldable
  import Data.Monoid

  data Tree a = Node a | Branch (Tree a) (Tree a)

  instance foldableTree :: Foldable Tree where
    foldr f z (Node x)     = x `f` z
    foldr f z (Branch l r) = foldr f (foldr f z r) l

    foldl f z (Node x)     = z `f` x
    foldl f z (Branch l r) = foldl f (foldl f z l) r

    foldMap f (Node x)     = f x
    foldMap f (Branch l r) = foldMap f l <> foldMap f r

  main = Debug.Trace.print <<< sum $ Branch (Node 1) (Branch (Node 2) (Node 3))
