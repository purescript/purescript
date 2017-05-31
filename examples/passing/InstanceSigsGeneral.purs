module Main where

import Control.Monad.Eff.Console (log)

class Eq a where
  eq :: a -> a -> Boolean

instance eqNumber :: Eq Number where
  eq :: forall x y. x -> y -> Boolean
  eq _ _ = true

main = log "Done"
