-- @shouldFailWith ErrorParsingModule
module Main where

error err = case err of \_ -> 1
