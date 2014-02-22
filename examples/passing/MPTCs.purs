module Main where

import Prelude
import Data.Array

class NullaryTypeClass where
  greeting :: String

instance NullaryTypeClass where
  greeting = "Hello, World!"

class Coerce a b where
  coerce :: a -> b

instance Coerce a a where
  coerce a = a

instance (Prelude.Show a) => Coerce a String where
  coerce = show

instance (Coerce a b) => Coerce [a] [b] where
  coerce = map coerce

main = Debug.Trace.print $ coerce [greeting] :: [String]
