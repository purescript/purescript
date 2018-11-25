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

type MyRecord r = { a :: Int | r }

rowContainsB :: forall r result
   . Row.Contains "b" r result
  => MyRecord r -> BProxy result
rowContainsB = rowContains (SProxy :: SProxy "b")

testC :: BProxy False
testC = rowContainsB { a: 0, c: 0 }

testD :: BProxy True
testD = rowContainsB { a: 0, b: 0 }

main = log "Done"
