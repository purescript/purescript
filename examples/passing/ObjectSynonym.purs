module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type Inner = Number

inner :: Inner
inner = 0.0

type Outer = { inner :: Inner }

outer :: Outer
outer = { inner: inner }

main = log "Done"
