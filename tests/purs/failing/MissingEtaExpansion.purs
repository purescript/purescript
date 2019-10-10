-- @shouldFailWith MissingEtaExpansion
-- Cf. passing/MissingEtaExpanson.purs and passing/UnifyInTypeInstanceLookup.purs
module Main where

f :: Int -> Int
f = f
