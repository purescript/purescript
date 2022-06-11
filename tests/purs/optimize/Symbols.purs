module Main where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons)
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

get
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => Proxy l
  -> Record r
  -> a
get l r = unsafeGet (reflectSymbol l) r

set
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => Proxy l
  -> a
  -> Record r
  -> Record r
set l a r = unsafeSet (reflectSymbol l) a r

foo :: Proxy "foo"
foo = Proxy

bar :: Proxy "bar"
bar = Proxy

f :: Int -> Int
f n = get foo { foo: n }

g :: Int -> Int
g n = get bar { foo: 0, bar: n }

h :: Int -> { foo :: Int }
h n = set foo n { foo: 0 }
