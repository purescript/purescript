-- @shouldFailWith OrphanRoleDeclaration
module Main where

type role D nominal
data D a = D a
