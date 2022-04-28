module Main where

import Prelude
import Prim.Row
import Effect
import Effect.Console
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

foreign import unsafeGet
  :: forall r a
   . String
  -> Record r
  -> a

foreign import unsafeSet
  :: forall r1 r2 a
   . String
  -> a
  -> Record r1
  -> Record r2

get
  :: forall r r' l a
   . IsSymbol l
  => Cons l a r' r
  => Proxy l
  -> Record r
  -> a
get l = unsafeGet (reflectSymbol l)

set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => Proxy l
  -> b
  -> Record r1
  -> Record r2
set l = unsafeSet (reflectSymbol l)

lens 
  :: forall l f r1 r2 r a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => Functor f
  => Proxy l
  -> (a -> f b)
  -> Record r1
  -> f (Record r2)
lens l f r = flip (set l) r <$> f (get l r)

getFoo :: forall a r. { foo :: a | r } -> a
getFoo = get (Proxy :: Proxy "foo")

setFoo :: forall a b r. b -> { foo :: a | r } -> { foo :: b | r }
setFoo = set (Proxy :: Proxy "foo")

fooLens :: forall f a b r. Functor f => (a -> f b) -> { foo :: a | r } -> f { foo :: b | r } 
fooLens = lens (Proxy :: Proxy "foo")

main :: Effect Unit
main = do
  _ <- fooLens logShow { foo: 1 }
  log (getFoo (setFoo "Done" { foo: 1 }))
