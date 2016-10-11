module Main where

import Prelude
import Control.Monad.Eff.Console (log)

newtype Bar' = Bar' Int

data Foo' = Foo' Bar'

data Baz'' = Baz'' | Baz'

f ∷ Foo' → Boolean
f a = case a of Foo' b → true

f' ∷ Boolean
f' = f $ Foo' $ Bar' 0

g ∷ Baz'' → Int
g Baz'' = 0
g Baz' = 1

g' ∷ Int
g' = g Baz''

h ∷ Bar' → Int
h (Bar' x)
 | x <= 10 = x * 2 
 | otherwise = 10

h' ∷ Int
h' = h $ Bar' 4

main = log "Done"