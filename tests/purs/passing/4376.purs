module Main where

import Prelude
import Prim.Row (class Union)

import Effect.Console (log)
import Type.Proxy (Proxy(..))

data Maybe a = Just a | Nothing

-- Make sure that record updates get monomorphized.
asNothing :: forall a. { a :: Maybe a } -> { a :: Maybe a }
asNothing = _ { a = Nothing }

union :: forall a b c. Union a b c => Record a -> Record b -> Proxy c
union _ _ = Proxy

-- This fails to solve if neither is monomorphized.
shouldSolve :: forall a b. Proxy ( a :: Maybe a, b :: Maybe b )
shouldSolve = { a: Nothing } `union` { b: Nothing }

main = log "Done"
