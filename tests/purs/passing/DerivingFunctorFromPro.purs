module Main where

-- Note that Data.Profunctor is not in the dependencies of any types imported
-- here. The package that contains that module must be a dependency of the test
-- project.

import Prelude

import Effect.Console (log)

data Test a
  = Test1 ((Array a -> Int) -> Int)
  | Test2 { f :: ({ a :: a } -> Int) -> Int }
derive instance Functor Test

main = log "Done"
