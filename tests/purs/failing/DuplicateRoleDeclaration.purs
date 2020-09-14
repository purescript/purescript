-- @shouldFailWith DuplicateRoleDeclaration
module Main where

data A a = A
type role A nominal
type role A phantom
