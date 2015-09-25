module Main where

import Prelude

data Bar a = Bar
data Baz

class Foo a where
  foo :: Bar a -> Baz

foo_ :: forall a. (Foo a) => a -> Baz
foo_ x = foo ((mkBar :: forall a. (Foo a) => a -> Bar a) x)

mkBar :: forall a. a -> Bar a
mkBar _ = Bar

main = Control.Monad.Eff.Console.log "Done"

