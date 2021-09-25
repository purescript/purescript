module Lib where

newtype Const :: forall k. Type -> k -> Type
newtype Const a b = Const a

data Unit = Unit

type CONST = Const
type UNIT = CONST Unit
