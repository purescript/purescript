-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

class MyShow a where
  myShow :: a -> String

instance cannotShowFunctions :: Fail "Cannot show functions" => MyShow (a -> b) where
  myShow _ = "unreachable"

main :: Eff _ _
main = log (myShow (_ + 1))
