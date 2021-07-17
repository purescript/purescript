-- @shouldFailWith RoleDeclarationArityMismatch
module Main where

foreign import data A :: Type -> (Type -> Type)
type role A nominal
