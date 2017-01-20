module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Array
import Data.Array.Partial as P
import Partial.Unsafe

length :: forall a. Array a -> Int
length = go 0 where
  go acc arr =
    if null arr
    then acc
    else go (acc + 1) (unsafePartial P.tail arr)

main = do
  logShow (length (1 .. 10000))
  log "Done"
