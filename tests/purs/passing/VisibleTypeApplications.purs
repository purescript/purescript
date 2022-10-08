module Main where

import Prelude
import Effect.Console (log)

foreign import data Id :: forall (a :: Type). a -> a

identityCheck :: forall (@f :: forall (a :: Type). a -> a). Int
identityCheck = 0

identityPass :: Int
identityPass = identityCheck @Id

foreign import data Const :: forall a b. a -> b -> a

constCheck :: forall (a :: Type) (@f :: forall (b :: Type). b -> a). Int
constCheck = 0

constPass :: Int
constPass = constCheck @(Const Int)

main = log "Done"
