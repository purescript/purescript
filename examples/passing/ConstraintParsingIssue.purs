module Main where

import Control.Monad.Eff.Console

class X a

instance x :: X (Array (Array a)) => X (Array a)

main = log "Done"
