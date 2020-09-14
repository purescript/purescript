-- @shouldFailWith RoleDeclarationArityMismatch
module Main where

data A = A
type role A nominal
