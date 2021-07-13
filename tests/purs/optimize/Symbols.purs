module Main where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

get
  :: forall proxy r r' l a
   . IsSymbol l
  => Cons l a r' r
  => proxy l
  -> Record r
  -> a
get l r = unsafeGet (reflectSymbol l) r

foo :: Proxy "foo"
foo = Proxy

f :: Int -> Int
f n = get foo { foo: n }

g :: Int -> Int
g n = get foo { foo: n, bar: 42 }
