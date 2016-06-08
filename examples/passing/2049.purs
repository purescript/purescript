module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data List a = Cons a (List a) | Nil

infixr 6 Cons as :

f :: List { x :: Int, y :: Int } -> Int
f ( r@{ x } : _) = x + r.y
f _ = 0

main = log "Done"
