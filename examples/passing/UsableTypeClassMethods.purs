-- this is testing that we don't see an `UnusableDeclaration` error for type
-- class methods that should be valid based on various configurations of fundeps
module Main where

import Control.Monad.Eff.Console (log)

-- no fundeps
class C0 a b where
  c0 :: a -> b

-- simple fundep
class C1 a b | a -> b where
  c1 :: a
  c1' :: a -> b

-- transitive
class C2 a b c | a -> b, b -> c where
  c2 :: a
  c2' :: a -> b
  c2'' :: a -> c
  c2''' :: a -> b -> c

-- with cycles
class C3 a b c | a -> b, b -> a, b -> c where
  c3 :: a
  c3' :: b
  c3'' :: a -> c
  c3''' :: b -> c
  c3'''' :: a -> b -> c

-- nullary class
class C4 where
  c4 :: forall a. a

main = log "Done"
