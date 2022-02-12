-- @shouldFailWith NoInstanceFound
module Main where

import Prim.Int (class DivMod)

data Proxy q r = Proxy

failing :: forall n. DivMod n 0 1 0 => Proxy n n
failing = Proxy

failing' :: Proxy _ _
failing' = failing
