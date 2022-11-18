module Main where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Profunctor (class Profunctor)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

newtype MonoAndBi a b = MonoAndBi (Effect Unit)
derive instance Functor (MonoAndBi a)
instance Bifunctor MonoAndBi where
  bimap _ _ _ = MonoAndBi (assert' "Bifunctor instance was used but the Functor instance was expected" false)

newtype Test1 a = Test1 (MonoAndBi Int a)
derive instance Functor Test1

data ExclusivelyBi a b
derive instance Bifunctor ExclusivelyBi

newtype Test2 a = Test2 (ExclusivelyBi Int a)
derive instance Functor Test2

newtype MonoAndPro a b = MonoAndPro (Effect Unit)
derive instance Functor (MonoAndPro a)
instance Profunctor MonoAndPro where
  dimap _ _ _ = MonoAndPro (assert' "Profunctor instance was used but the Functor instance was expected" false)

newtype Test3 a = Test3 (MonoAndPro Int a)
derive instance Functor Test3

data ExclusivelyPro a b
derive instance Profunctor ExclusivelyPro

newtype Test4 a = Test4 (ExclusivelyPro Int a)
derive instance Functor Test4

main = do
  let t = Test1 (MonoAndBi (pure unit))
  let Test1 (MonoAndBi result1) = map identity t
  result1
  let t = Test3 (MonoAndPro (pure unit))
  let Test3 (MonoAndPro result3) = map identity t
  result3
  log "Done"
