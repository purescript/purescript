-- @shouldFailWith UnsupportedRoleDeclaration
module Main where

class C a
type role C representational
