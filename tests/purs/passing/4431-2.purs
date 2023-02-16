module Main where

import Prelude
import Data.Const (Const)
import Effect.Console (log)

data TypedCache :: (Type -> Type) -> Type -> Type
data TypedCache key a = Get (key a)

derive instance Functor (TypedCache (Const k))

main = log "Done"
