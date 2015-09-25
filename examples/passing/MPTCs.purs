module Main where

import Prelude

class NullaryTypeClass where
  greeting :: String

instance nullaryTypeClass :: NullaryTypeClass where
  greeting = "Hello, World!"

class Coerce a b where
  coerce :: a -> b

instance coerceRefl :: Coerce a a where
  coerce a = a

instance coerceShow :: (Prelude.Show a) => Coerce a String where
  coerce = show

main = Control.Monad.Eff.Console.log "Done"
