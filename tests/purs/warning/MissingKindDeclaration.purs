-- @shouldWarnWith MissingKindDeclaration
-- @shouldWarnWith MissingKindDeclaration
-- @shouldWarnWith MissingKindDeclaration
-- @shouldWarnWith MissingKindDeclaration
module Main where

data Proxy a = Proxy

newtype F a b = F b

type Natural f g = forall a. f a -> g a

class Clazz a b c
