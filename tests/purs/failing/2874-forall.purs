-- @shouldFailWith ErrorParsingModule
module Main where

class T a b | a -> b
instance tT :: (T Int (forall a. a)) => T Int String

ddd :: Int
ddd = 0 :: forall t. T Int t => Int
