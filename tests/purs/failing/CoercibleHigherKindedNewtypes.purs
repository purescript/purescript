-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

newtype Ap f a b = Ap (f a b)

data Tuple a b = Tuple a b
newtype N1 a b = N1 (Tuple a b)
newtype N2 b a = N2 (Tuple a b)

swap :: Ap N1 Int String -> Ap N2 Int String
swap = coerce
