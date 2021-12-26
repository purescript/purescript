-- @shouldWarnWith MissingKindDeclaration
module Main where

type T f a = f (f a)
