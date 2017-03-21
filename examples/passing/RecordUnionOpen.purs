module Main where

import Prelude
import Data.Record (merge)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Unsafe.Coerce (unsafeCoerce)

type Mand r = (title :: String | r)
type Opt eff r = (onClick :: Unit -> Eff eff Unit | r)

withDefaults :: forall xtra all eff
   . Normalised (Mand (Opt eff all)) (Mand (Opt eff xtra))
  => Record (Mand all) -> Record (Mand (Opt eff xtra))
withDefaults = merge ({onClick: \_ -> pure unit } :: Record (Opt eff ()))

leftClosed :: forall r. Normalised (left::Int|r) (left::Int|r) => {left::Int} -> {|r} -> {left::Int|r}
leftClosed = merge

rightClosed :: forall l. Normalised (right::Int|l) (right::Int|l) => {|l} -> {right::Int} -> {right::Int|l}
rightClosed = merge

bothOpen :: forall l r u1 u2 c
   .
   Union r l u1
  => Union l r u2
  => Normalised (left::Int, right::Int|u1) (left::Int,right::Int|c)
  => Normalised (left::Int, right::Int|u2) (left::Int,right::Int|c)
  =>
  {left::Int|l} -> {right::Int|r} -> {left::Int,right::Int|c}
bothOpen = merge

main :: Eff (console::CONSOLE) Unit
main = do
  let withoutClick = withDefaults {title:"Title", totes:""}
      withClick = withDefaults {title:"Title", onClick: \_ -> log "Done", extraBits:""}
      compare :: forall l r. (l -> r -> {left::Int,right::Int,three::Int,four::Int}) -> l -> r -> Eff (console::CONSOLE) Unit
      compare f l r = case f l r of
        {left:1,right:2,three:3,four:4} -> pure unit
        p -> log $ unsafeCoerce p
  compare leftClosed {left:1} {right:2, three:3, four:4}
  compare rightClosed {left:1, three:3, four:4} {right:2}
  compare bothOpen {left:1, four:1, three:0} {right:2, three:3, four:4}
  withoutClick.onClick unit
  withClick.onClick unit
