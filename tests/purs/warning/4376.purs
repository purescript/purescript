-- @shouldWarnWith MissingTypeDeclaration
module Main where

data Maybe a = Just a | Nothing

value = Nothing
