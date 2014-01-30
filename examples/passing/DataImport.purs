module M1 where

data Test = Test String

module Main where

import M1

unTest (Test s) = s

main = do
  let x = unTest (Test "Done")
  Trace.trace x
