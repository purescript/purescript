module Lib where

data T :: forall m. m -> Type
data T msg = E

type TAlias :: forall k. k -> Type
type TAlias msg = T msg
