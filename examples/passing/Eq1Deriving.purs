module Main where

import Prelude
import Data.Eq (class Eq1, eq1)
import Control.Monad.Eff.Console (log)

data Foo a
  = Foo a
  | Bar a a
  | Baz

derive instance eq1Foo :: Eq1 Foo
derive instance eqFoo :: Eq a => Eq (Foo a)

no :: Boolean
no = Foo true == Bar true true

no1 :: Boolean
no1 = Foo true `eq1` Bar true true

data FooT f a
  = FooT (f a)
  | BarT (f a) a
  | BazT

derive instance eq1FooT :: Eq1 f => Eq1 (FooT f)
derive instance eqFooT :: (Eq1 f, Eq a) => Eq (FooT f a)

noT :: Boolean
noT = FooT (Foo true) == BarT (Foo true) true

noT1 :: Boolean
noT1 = FooT (Foo true) `eq1` BarT (Foo true) true

main = log "Done"
