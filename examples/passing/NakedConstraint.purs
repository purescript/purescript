module Main where

import Control.Monad.Eff.Console

data List a = Nil | Cons a (List a)

head :: (Partial) => List Int -> Int
head (Cons x _) = x

main = log "Done"
