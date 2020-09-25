-- @shouldFailWith UnsupportedRoleDeclaration
module Main where

data A a = A

type B a = A a
type role B nominal
