module Main where

import Prelude
import Data.Record (class RowUnion, merge)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Unsafe.Coerce (unsafeCoerce)

type Mand r = {title :: String | r}
type Opt eff r = (onClick :: Unit -> Eff eff Unit | r)

defaultValues :: forall eff. {|Opt eff ()}
defaultValues = {onClick: \_ -> pure unit }

withDefaults :: forall xtra all eff. RowUnion (Opt eff ()) all (Opt eff xtra) => Mand all -> Mand (Opt eff xtra)
withDefaults = merge defaultValues

leftClosed :: forall r. {left::Int} -> {|r} -> {left::Int|r}
leftClosed = merge

rightClosed :: forall l. {|l} -> {right::Int} -> {right::Int|l}
rightClosed = merge

bothOpen :: forall l r b. RowUnion r l b => {left::Int|l} -> {right::Int|r} -> {left::Int,right::Int|b}
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
  compare bothOpen {left:1, three:0, four:1} {four:4, right:2, three:3}
  withoutClick.onClick unit
  withClick.onClick unit
