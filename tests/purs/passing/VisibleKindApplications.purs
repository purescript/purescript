module Main where

import Effect.Console (log)
import Type.Proxy (Proxy(..))

foreign import data IdK :: forall a. a -> a

type IntId :: Int -> Int
type IntId = IdK @Int

type NumberId :: Number -> Number
type NumberId = IdK @Number

main = log "Done"
