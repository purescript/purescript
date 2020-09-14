-- @shouldFailWith RoleDeclarationArityMismatch
module Main where

foreign import data A :: Type
type role A nominal
