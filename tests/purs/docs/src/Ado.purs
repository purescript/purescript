-- See https://github.com/purescript/purescript/issues/3414 
module Ado where
  
test = 
  ado x <- 1
   in x

  where
  map f x = f x
