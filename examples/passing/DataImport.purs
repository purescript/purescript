module M1 where

data Test = MkTest String

module Main where

import M1

unTest (MkTest s) = s

main = do
  let x = unTest (MkTest "Done")
  Trace.trace x
