module Main where

import Prelude
import Control.Monad.Eff.Console

foreign import merge 
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record r1
  -> Record r2
  -> Record r3

test1 = merge { x: 1 } { y: true }

test2 = merge { x: 1 } { x: true }

mergeWithExtras
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record ( x :: Int | r1 )
  -> Record ( y :: Boolean | r2 )
  -> Record ( x :: Int, y :: Boolean | r3)
mergeWithExtras = merge

test3 x = merge { x: 1 } x :: { x :: Int, y :: Boolean }

main = do
  logShow test1.x
  logShow test1.y
  logShow (test1.x == 1)
  logShow (mergeWithExtras { x: 1 } { y: true, z: 42.0 }).x
  log "Done"
