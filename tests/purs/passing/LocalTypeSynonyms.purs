module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

foo :: forall a. a -> { left :: Int, right :: a }
foo a = result
  where
  result = result'
    where
    type Bravo a = { left :: a, right :: Alpha }
    result' :: Bravo Int
    result' = { left: 0, right: a }
  type Alpha = a

bar :: Effect Int
bar = do
  log "hello"
  let type Alpha = Int
  pure (1 :: Alpha)

baz :: forall r a. { a :: a | r } -> { a :: a, b :: Int }
baz = f
  where
  type In = { a :: a | r }
  type Out = { a :: a, b :: Int }
  f :: In -> Out
  f { a } = { a, b: 0 }

kinded :: Effect Int
kinded = do
  let type Alpha (a :: Type) = Type
  let type Bravo (a :: Alpha String -> Type) = a Unit
  let type Charlie (a :: Alpha String) = Int
  pure (1 :: Bravo Charlie)


intentionallyUntyped = wrap thing
  where
  type Wrapped a = { wrapped :: a }
  wrap :: forall a. a -> Wrapped a
  wrap wrapped = { wrapped }
  thing :: Int
  thing = 0


main :: Effect Unit
main = if intentionallyUntyped.wrapped == 0
  then log "Done"
  else log "Fail"
