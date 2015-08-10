module Main where

import Prelude

length :: forall a. Array a -> Int
length _ = 0

type Foo a = Array a

foo _ = length ([] :: Foo Number)

main = Control.Monad.Eff.Console.log "Done"
