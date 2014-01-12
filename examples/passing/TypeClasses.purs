module TypeClasses where

class Show a where
  show :: a -> String

instance Show String where
  show s = s
