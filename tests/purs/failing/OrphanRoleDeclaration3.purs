-- @shouldFailWith OrphanRoleDeclaration
module Main where

data D1 a = D1 a

data D2 a = D2 a

type role D1 nominal
