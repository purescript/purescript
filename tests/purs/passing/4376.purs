module Main where

import Prelude
import Prim.Row (class Union)

import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- Make sure that record updates get monomorphized.
asNothing :: forall a. { a :: Maybe a } -> { a :: Maybe a }
asNothing = _ { a = Nothing }

union :: forall a b c. Union a b c => Record a -> Record b -> Proxy c
union _ _ = Proxy

-- This fails to solve if neither is monomorphized.
shouldSolve :: forall a b. Proxy ( a :: Maybe a, b :: Maybe b )
shouldSolve = { a: Nothing } `union` { b: Nothing }

-- Removes ConstrainedTypeUnified
v1 :: { a :: Maybe Unit }
v1 = { a : Just unit }

v2 :: { a :: Maybe Unit }
v2 = let v3 = v1 { a = mempty } in v3

main = log "Done"
