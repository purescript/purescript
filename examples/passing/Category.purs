module Category where

class Category c where
  id :: forall x. c x x
  (<|) :: forall x y z. c y z -> c x y -> c x z

instance Category (->) where
  id = Prelude.id
  (<|) = Prelude.(<|)

module Main where

import Category
  
main = Trace.trace (id "Done")
