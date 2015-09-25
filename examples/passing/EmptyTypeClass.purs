module Main where

import Prelude

class Partial

head :: forall a. (Partial) => Array a -> a
head [x] = x

instance allowPartials :: Partial

main = Control.Monad.Eff.Console.log $ head ["Done"]
