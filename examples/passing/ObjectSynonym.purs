module Main where

import Prelude

type Inner = Number

inner :: Inner
inner = 0.0

type Outer = { inner :: Inner }

outer :: Outer
outer = { inner: inner }

main = Control.Monad.Eff.Console.log "Done"
