module Do where

test1 = do let x = 1

test2 y = do x <- y

test3 = do ret 1
           ret 2
