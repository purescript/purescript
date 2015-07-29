module Main where

import Prelude ()

($) :: forall a b. (a -> b) -> a -> b
($) f x = f x

infixr 1000 $

id x = x

test1 x = id $ id $ id $ id $ x

test2 x = id id $ id x

main = Control.Monad.Eff.Console.log "Done"
