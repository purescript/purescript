-- @shouldFailWith ErrorParsingModule
module Main where

type NotAllowed @f @a = f a
