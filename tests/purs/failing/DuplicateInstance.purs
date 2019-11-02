-- @shouldFailWith DuplicateInstance
module Main where
class X
class Y
instance i :: X
instance i :: Y
