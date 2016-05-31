module Main where

import Control.Monad.Eff.Console (log)

type List a = forall r. r -> (a -> r -> r) -> r

empty :: forall a. List a
empty = \r f -> r

cons :: forall a. a -> List a -> List a
cons = \a l r f -> f a (l r f)

append :: forall a. List a -> List a -> List a
append = \l1 l2 r f -> l2 (l1 r f) f

test = append (cons 1 empty) (cons 2 empty)

main = log "Done"
