module Main where

import Control.Monad.Eff.Console (log)

applyFn :: forall a b. (a -> b) -> a -> b
applyFn f x = f x

infixr 1000 applyFn as $

id x = x

test1 x = id $ id $ id $ id $ x

test2 x = id id $ id x

main = log "Done"
