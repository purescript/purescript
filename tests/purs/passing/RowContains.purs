module Main where

import Effect.Console (log)
import Prim.Row as Row
import Type.Prelude (BProxy(..), SProxy(..), True, False)

rowContains
  :: forall proxy row label result
   . Row.Contains label row result
  => SProxy label
  -> proxy row
  -> BProxy result
rowContains _ _ = BProxy

testA :: BProxy True
testA = rowContains (SProxy :: SProxy "a") { a: 1 }

testB :: BProxy False
testB = rowContains (SProxy :: SProxy "a") {}

main = log "Done"
