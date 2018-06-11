module Lib (class X, x) where

class X a where
  x :: a -> String

class Y a

instance xArray :: Y a => X (Array a) where
  x _ = "[]"
