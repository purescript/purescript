module Main where

import Prelude ()

fns = \f -> if f true then f else \x -> x

not = \x -> if x then false else true

main = Control.Monad.Eff.Console.log "Done"
