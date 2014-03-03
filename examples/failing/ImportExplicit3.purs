module M1 where

  data X = Y

module Main where

  import M1 (X(Z, Q))
