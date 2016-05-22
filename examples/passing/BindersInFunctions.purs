module Main where

import Prelude
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

snd :: forall a. Partial => Array a -> a
snd = \[_, y] -> y

main :: Eff _ _
main = do
  let ts = unsafePartial (snd [1.0, 2.0])
  assert' "Incorrect result from 'snd'." (ts == 2.0)
  log "Done"
