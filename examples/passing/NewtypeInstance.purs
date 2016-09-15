module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

newtype X = X String

derive newtype instance showX :: Show X

derive newtype instance eqX :: Eq X

derive newtype instance ordX :: Ord X

newtype Y a = Y (Array a)

derive newtype instance showY :: Show (Y String)

class Singleton a b where
  singleton :: a -> b

instance singletonArray :: Singleton a (Array a) where
  singleton x = [x]

derive newtype instance singletonY :: Singleton a (Y a)

main = do
  logShow (X "test")
  logShow (singleton "test" :: Y String)
  log "Done"
