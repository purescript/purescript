module Main where

import Prelude
import Effect.Console (log)

import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int as PI
import Prim.Ordering as PO
import Type.Proxy (Proxy)

-- | Valid bounds are between 1 and 5
newtype N = N Int

type MinN :: Int
type MinN = 1

type MaxN :: Int
type MaxN = 5

mkN
  :: forall i lower upper
   . Reflectable i Int
  => PI.Add MinN (-1) lower
  => PI.Compare i lower PO.GT
  => PI.Add MaxN 1 upper
  => PI.Compare i upper PO.LT
  => Proxy i
  -> N
mkN p = N (reflectType p)

data Something = Something N Int

mkSomething
  :: forall i lower upper
   . Reflectable i Int
  => PI.Add MinN (-1) lower
  => PI.Compare i lower PO.GT
  => PI.Add MaxN 1 upper
  => PI.Compare i upper PO.LT
  => Proxy i
  -> Int
  -> Something
mkSomething p i = Something (mkN p) i

main = log "Done"
