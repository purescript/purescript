module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

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
  => RowCons l a r' r
  => SProxy l
  -> Record r
  -> a
get l = unsafeGet (reflectSymbol l)

set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> Record r1
  -> Record r2
set l = unsafeSet (reflectSymbol l)

lens 
  :: forall l f r1 r2 r a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => Functor f
  => SProxy l
  -> (a -> f b)
  -> Record r1
  -> f (Record r2)
lens l f r = flip (set l) r <$> f (get l r)

getFoo :: forall a r. { foo :: a | r } -> a
getFoo = get (SProxy :: SProxy "foo")

setFoo :: forall a b r. b -> { foo :: a | r } -> { foo :: b | r }
setFoo = set (SProxy :: SProxy "foo")

fooLens :: forall f a b r. Functor f => (a -> f b) -> { foo :: a | r } -> f { foo :: b | r } 
fooLens = lens (SProxy :: SProxy "foo")

main :: Eff (console :: CONSOLE) Unit
main = do
  _ <- fooLens logShow { foo: 1 }
  log (getFoo (setFoo "Done" { foo: 1 }))
