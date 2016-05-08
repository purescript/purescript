module Main where

import A (type (~>), type (/\), (/\))

natty ∷ ∀ f. f ~> f
natty x = x

data Compose f g a = Compose (f (g a))

testPrecedence ∷ ∀ f g. Compose f g ~> Compose f g
testPrecedence x = x

swap ∷ ∀ a b. a /\ b → b /\ a
swap (a /\ b) = b /\ a

main = Control.Monad.Eff.Console.log "Done"
