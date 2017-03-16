module Main where

import Prelude
import Data.Record (merge)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)

type Mand r = (title :: String | r)
type Opt eff r = (onClick :: Unit -> Eff eff Unit | r)

withDefaults :: forall o eff
   . Normalised (Mand (Opt eff o)) (Mand (Opt eff ()))
  => Record (Mand o) -> Record (Mand (Opt eff ()))
withDefaults = merge ({onClick: \_ -> pure unit } :: Record (Opt eff ()))

main :: forall eff. Eff (console::CONSOLE|eff) Unit
main = do
  let withoutClick = withDefaults {title:"Title"}
      withClick = withDefaults {title:"Title", onClick: \_ -> log "Done"}
  withoutClick.onClick unit
  withClick.onClick unit
