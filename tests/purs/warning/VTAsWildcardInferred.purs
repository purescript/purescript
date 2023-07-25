-- See https://github.com/purescript/purescript/issues/4487
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

f :: forall @a. a -> a
f = identity

test1 :: { x :: Int }
test1 = f @{ x :: _ } { x: 42 }

class Foo :: Type -> Type -> Type -> Constraint
class Foo a b c | a -> b c where
  fooMember :: a -> b

wrap :: forall @a. Array a -> Array (Array a)
wrap as = [as]

arrFooMember :: forall c. Array (Foo Int Boolean c => Int -> Boolean)
arrFooMember = [fooMember]

test2 :: forall c. Array (Array (Foo Int Boolean c => Int -> Boolean))
test2 = wrap @(Foo Int Boolean _ => _) arrFooMember -- neither wildcard should warn IMO

main :: Effect Unit
main = log "Done"
