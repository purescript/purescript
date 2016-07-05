module Main where

import Control.Monad.Eff.Console (log)

foreign import a' :: Number
foreign import b' :: Number
foreign import c' :: Number
foreign import d' :: Number

main = log "Done"
