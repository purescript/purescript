module ImportsSpec1
       ( exportedFunction
       , MyType
       , MyParamType
       , MyNewtype(..)
       , MyMaybe(..)
       , SpecialCase
       , X(..)
       , class ATypeClass
       , typeClassFun
       , OnlyTypeExported
       )
       where

exportedFunction ∷ ∀ a. a → a
exportedFunction x = x

type MyType = String

type MyParamType a = Array a

newtype MyNewtype = MyNewtype String

data MyMaybe a = MyJust a | MyNothing

data SpecialCase
data X = SpecialCase

newtype OnlyTypeExported = OnlyTypeExported String

class ATypeClass a where
  typeClassFun ∷ a -> a
