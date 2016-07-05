module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Foo a b

class Bar a c

class (Foo a b, Bar a c) <= Baz a b c

instance foo :: Foo (a -> b) a

instance bar :: Bar (a -> b) b

instance baz :: (Eq a) => Baz (a -> b) a b

main = log "Done"
