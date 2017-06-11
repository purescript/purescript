module Main where

import Control.Monad.Eff.Console (log)

data Void

absurd :: ∀ a. Void -> a
absurd = case _

absurd' :: ∀ a. Void -> Void -> a
absurd' a b = case a, b

main = log "Done"
