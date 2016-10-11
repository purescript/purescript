module Main where

import Prelude
import Control.Monad.Eff.Console (log)

newtype Bar' = Bar' Int

data Foo' = Foo' Bar'

data Baz'' = Baz'' | Baz'

f :: Foo' -> Boolean
f a = case a of Foo' b -> true

g :: Baz'' -> Int
g Baz'' = 0
g Baz' = 1
 
h :: Bar' -> Int
h (Bar' x)
 | x <= 10 = x * 2 
 | otherwise = 10

main = log "Done"