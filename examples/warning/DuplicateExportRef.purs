-- @shouldWarnWith DuplicateExportRef
-- @shouldWarnWith DuplicateExportRef
-- @shouldWarnWith DuplicateExportRef
-- @shouldWarnWith DuplicateExportRef
-- @shouldWarnWith DuplicateExportRef
-- @shouldWarnWith DuplicateExportRef
-- @shouldWarnWith DuplicateExportRef
module Main
  ( X(X, X), X
  , fn, fn
  , (!), (!)
  , class Y, class Y
  , Natural, type (~>), type (~>)
  , module Prelude, module Prelude
  ) where

import Prelude (Unit)

data X = X

fn :: X -> X -> X
fn _ _ = X

infix 2 fn as !

class Y a

type Natural f g = forall a. f a -> g a

infixl 1 type Natural as ~>
