module Clash2a where

value :: String
value = "hello"

type Type = String

class TypeClass a b where
  typeClassMember :: a -> b
