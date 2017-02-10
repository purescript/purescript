-- @shouldFailWith ErrorParsingModule
module Main where

import Prelude

-- wrong binders for function, the first one should be VarBinder
x =
  let (X a b) x y = hoge
  in
   a
