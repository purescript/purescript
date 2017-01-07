-- @shouldWarnWith MissingTypeDeclaration
module Main where

import Control.Monad.Eff.Console

type T = forall a. Array a

foo :: T
foo = bar where
  bar :: forall a. Array a
  bar = []

main = log "Done"
