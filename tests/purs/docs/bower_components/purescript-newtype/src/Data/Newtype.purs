module Data.Newtype where

class Newtype t a | t -> a where
  wrap :: a -> t
  unwrap :: t -> a
