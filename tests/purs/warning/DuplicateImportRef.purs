-- @shouldWarnWith DuplicateImportRef
-- @shouldWarnWith DuplicateImportRef
-- @shouldWarnWith DuplicateImportRef
-- @shouldWarnWith DuplicateImportRef
module Main where

import Prelude
  ( Unit, Unit
  , unit, unit
  , class Functor, class Functor
  , (<>), (<>)
  )

u :: Unit
u = unit <> unit

fid :: forall f a. Functor f => f a -> f a
fid fa = fa
