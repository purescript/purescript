module Iota where

  s = \x -> \y -> \z -> x z (y z)

  k = \x -> \y -> x

  iota = \x -> x s k
