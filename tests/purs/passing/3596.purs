module Main where

import Prelude
import Effect (Effect)
import Effect.Console (error, log)

class Foo a where
  isEquatable :: a -> Boolean

data Bar = Bar

instance barFoo :: Foo Bar where
  isEquatable _ = false

instance equatableFoo :: (Eq a) => Foo a where
  isEquatable _ = true

main :: Effect Unit
main = do
  when (isEquatable Bar) $ error "wrong instance"
  log "Done"
