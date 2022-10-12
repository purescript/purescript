module Lib where

import Prelude

data Tuple a b = Tuple a b

infixr 6 Tuple as /\
infixr 6 type Tuple as /\

class Test a where
  runTest :: a -> String

instance Test Int where
  runTest _ = "4"

instance (Test a, Test b) => Test (a /\ b) where
  runTest (a /\ b) = runTest a <> runTest b
