-- @shouldFailWith InfiniteKind

module InfiniteKind2 where

data Tree m = Tree (m Tree)
