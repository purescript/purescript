-- @shouldFailWith RoleDeclarationArityMismatch
module Main where

type To = Function

foreign import data A :: To Type (To Type Type)
type role A nominal
