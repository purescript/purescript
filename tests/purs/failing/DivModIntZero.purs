-- @shouldFailWith NoInstanceFound
module Main where

import Prim.Int (class DivMod)

data Proxy q r = Proxy

failing :: forall q r. DivMod 10 0 q r => Proxy q r
failing = Proxy

failing' :: Proxy _ _
failing' = failing
