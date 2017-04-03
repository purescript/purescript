module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Stream a = Cons a (Stream a)

step :: forall a. Stream a -> Stream a
step (Cons _ xs) = xs

head :: forall a. Stream a -> a
head xs | Cons x _ <- step xs = x

main = log "Done"
