-- @shouldFailWith ErrorParsingModule
module Main where

class X a b | a -> b
class X a (forall t. t) <= Y a b | a -> b
instance tX :: X Int String
instance tY :: Y Int Boolean

ggg :: Int
ggg = 0 :: forall t. Y Int t => Int
