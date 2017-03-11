module Main where

import Prelude
import Control.Monad.Eff.Console
import Data.Array.Partial
import Partial.Unsafe

sumTCObug = go id where 
  go f 0 = f 
  go f n =
    let
      f' a = n + a
    in
      go f' 0

sumTCObug' = go id where 
  go f 0 = f
  go f n = go (\a -> n + a) 0

count :: forall a. (a -> Boolean) -> Array a -> Int
count p = count' 0 where
  count' acc [] = acc
  count' acc xs =
    let h = unsafePartial head xs
    in count' (acc + if p h then 1 else 0) (unsafePartial tail xs)

main = do
  let x = sumTCObug 7 3
      y = sumTCObug' 7 3
      z = count (_ > 0) [-1, 0, 1]
  logShow x
  logShow y 
  logShow z
  if x == 10 && y == 10 && z == 1
    then log "Done"
    else log "Fail"
