module List where

  data List a 
    = Nil 
    | Cons
      { head :: a
      , tail :: List a
      }

  isNull Nil = true
  isNull _ = false

  concat Nil l2 = l2
  concat (Cons x) l2 = Cons
    { head: x.head
    , tail: concat x.tail l2 
    }

  flatten Nil = Nil
  flatten (Cons cons) = concat cons.head (flatten cons.tail)
    
module Main where

main = Trace.trace "Done"
