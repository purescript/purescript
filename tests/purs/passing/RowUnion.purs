module Main where

import Prelude
import Prim.Row
import Effect
import Effect.Console

data Proxy a = Proxy

solve :: forall l r u. Union l r u => Proxy r -> Proxy u -> Proxy l
solve _ _ = Proxy

solveUnionBackwardsNil :: Proxy _
solveUnionBackwardsNil = solve (Proxy :: Proxy ()) (Proxy :: Proxy ())

solveUnionBackwardsCons :: Proxy _
solveUnionBackwardsCons = solve (Proxy  :: Proxy ( a :: Int )) (Proxy :: Proxy ( a :: Int, b :: String ))

solveUnionBackwardsDblCons :: Proxy _
solveUnionBackwardsDblCons = solve (Proxy :: Proxy ( a :: Int, a :: String )) (Proxy :: Proxy ( a :: Boolean, a :: Int, a :: String ))

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
  . Union r1 (y :: Boolean | r2) (y :: Boolean | r3)
 => { x :: Int | r1 }
 -> { y :: Boolean | r2 }
 -> { x :: Int, y :: Boolean | r3}
mergeWithExtras = merge

test3 x = merge { x: 1 } x
test3' x = merge x { x: 1 }

type Mandatory r = (x :: Int | r)
type Optional r = (x :: Int, y :: Int, z :: Int | r)

withDefaults
  :: forall r s
   . Union r (y :: Int, z :: Int) (y :: Int, z :: Int | s)
  => Record (Mandatory r)
  -> Record (Optional s)
withDefaults p = merge p { y: 1, z: 1 }

withDefaultsClosed
  :: forall r s
   . Union r (y :: Int, z :: Int) (y :: Int, z :: Int | s)
  => Subrow s (y :: Int, z :: Int)
  => Record (Mandatory r)
  -> Record (Optional s)
withDefaultsClosed p = merge p { y: 1, z: 1 }

test4 = withDefaults { x: 1, y: 2 }

-- r is a subrow of s if Union r t s for some t.
class Subrow (r :: # Type) (s :: # Type)
instance subrow :: Union r t s => Subrow r s

main :: Effect Unit
main = do
  logShow test1.x
  logShow test1.y
  logShow (test1.x == 1)
  logShow (mergeWithExtras { x: 1 } { x: 0, y: true, z: 42.0 }).x
  logShow (withDefaults { x: 1 }).x
  logShow (withDefaults { x: 1 }).y
  logShow (withDefaults { x: 1 }).z
  logShow (withDefaults { x: 1, y: 2 }).x
  logShow (withDefaults { x: 1, y: 2 }).y
  logShow (withDefaults { x: 1, y: 2 }).z
  logShow (withDefaultsClosed { x: 1, y: 2 }).x
  logShow (withDefaultsClosed { x: 1, y: 2 }).y
  logShow (withDefaultsClosed { x: 1, y: 2 }).z
  log "Done"
