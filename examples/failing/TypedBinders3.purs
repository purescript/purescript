-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

test = case 1 of
  (0 :: String) -> true
  _ -> false

main = do
  let t = test
  Control.Monad.Eff.Console.log "Done"
