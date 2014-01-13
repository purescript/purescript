module TypeClasses where

class Show a where
  show :: a -> String

instance TypeClasses.Show String where
  show s = s
