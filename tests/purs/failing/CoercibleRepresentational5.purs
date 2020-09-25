-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data MutuallyRecursiveRepresentational1 a
  = MutuallyRecursiveRepresentational1 a (MutuallyRecursiveRepresentational2 a)

type MutuallyRecursiveRepresentational1Synonym a = MutuallyRecursiveRepresentational1 a

data MutuallyRecursiveRepresentational2 a
  = MutuallyRecursiveRepresentational2 (MutuallyRecursiveRepresentational1Synonym a)

representationalToRepresentational :: MutuallyRecursiveRepresentational2 Int -> MutuallyRecursiveRepresentational2 String
representationalToRepresentational = coerce
