module Do where

id = { ret: \x -> x, bind: \x f -> f x }

test1 = id do let x = 1

test2 y = id do x <- y

test3 = id do return 1
              return 2
