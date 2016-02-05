module Main where

import Prelude

subtractOne :: Int -> Int
subtractOne = (_ - 1)

addOne :: Int -> Int
addOne = (1 + _)

named :: Int -> Int
named = (_ `sub` 1)

main = Control.Monad.Eff.Console.log "Done"
