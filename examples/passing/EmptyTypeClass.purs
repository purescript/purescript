module Main where

import Prelude

class Partial

head :: forall a. (Partial) => [a] -> a
head (x:xs) = x

instance allowPartials :: Partial

main = Debug.Trace.trace $ head ["Done"]
