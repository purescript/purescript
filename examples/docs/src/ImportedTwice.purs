-- See also an example in the wild: purescript-transformers v0.8.4.
-- Control.Monad.RWS.Trans re-exports `lift` from both Control.Monad.Trans
-- (where it is originally defined) and Control.Monad.RWS.Class (which
-- re-exports it from Control.Monad.Trans).

module ImportedTwice
  ( module A
  , module B
  )
  where

import A
import B

module A
  ( module B )
  where

import B

bar :: Int
bar = 1

module B where

foo :: Int
foo = 0
