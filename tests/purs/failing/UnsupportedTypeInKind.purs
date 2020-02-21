-- @shouldFailWith ErrorParsingModule
module Main where

class Ok
instance ok :: Ok

type S :: Ok => Type
type S = Int
