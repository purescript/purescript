module Main where

import A (type (~>), type (/\), (/\))
import Control.Monad.Eff.Console (log)

natty ∷ ∀ f. f ~> f
natty x = x

swap ∷ ∀ a b. a /\ b → b /\ a
swap (a /\ b) = b /\ a

main = log "Done"
