module Main where

import Prelude
import Effect.Console (log)

class NullaryTypeClass where
  greeting :: String

instance nullaryTypeClass :: NullaryTypeClass where
  greeting = "Hello, World!"

class Coerce a b where
  coerce :: a -> b

instance coerceShow :: Show a => Coerce a String where
  coerce = show
else
instance coerceRefl :: Coerce a a where
  coerce a = a

main = log "Done"
