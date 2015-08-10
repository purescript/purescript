module M1 where

import M2

foo = M3.baz

module M2 where

bar = M3.baz

module M3 where

baz = 1

module Main where

main = Control.Monad.Eff.Console.log "Done"
