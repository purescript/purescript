-- @shouldFailWith AdoLetNotYetSupported
module Main where

import Prelude

test1 = ado
  let x = 1
  in x
