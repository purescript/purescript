-- @shouldFailWith InfiniteKind

module InfiniteKind2 where

data Tree m a = Tree a (m (Tree a))
