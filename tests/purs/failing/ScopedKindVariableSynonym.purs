-- @shouldFailWith UndefinedTypeVariable
module Main where

type A x = forall a. a -> x -> Type

type B :: forall x. A x
type B y z = a
