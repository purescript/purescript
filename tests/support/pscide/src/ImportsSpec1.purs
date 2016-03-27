module ImportsSpec1 where

exportedFunction x = x

type MyType = String

newtype MyNewtype = MyNewtype String

data MyMaybe a = MyJust a | MyNothing

data SpecialCase
data X = SpecialCase

class ATypeClass a where
  typeClassFun :: a -> a
