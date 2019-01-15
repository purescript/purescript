module Main where

import Effect.Console

import Data.Symbol (SProxy(..))
import Prim.Boolean as Boolean
import Prim.Symbol as Symbol
import Type.Data.Boolean (BProxy(..))

symbolContains
  :: forall pattern sym result
   . Symbol.Contains pattern sym result
  => SProxy pattern
  -> SProxy sym
  -> BProxy result
symbolContains _ _ = BProxy

-- inferred type:
resultContains1 :: BProxy Boolean.True
resultContains1 = symbolContains (SProxy :: SProxy "b") (SProxy :: SProxy "abc")

-- inferred type:
resultContains2 :: BProxy Boolean.False
resultContains2 = symbolContains (SProxy :: SProxy "z") (SProxy :: SProxy "abc")

main = log "Done"
