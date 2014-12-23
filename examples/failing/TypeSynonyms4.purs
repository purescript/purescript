module TypeSynonyms4 where

type F x y = x -> y

type G x = F x

f :: G String String -> String
f k = k "Done"
