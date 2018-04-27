module Main where

import Prelude
import Effect.Console (log)

example1 :: String
example1 = do
  "Do"
  " notation"
  " for"
  " Semigroup"
  where
  discard x f = x <> f unit

applySecond :: forall f a b. Apply f => f a -> f b -> f b
applySecond fa fb = const identity <$> fa <*> fb

infixl 4 applySecond as *>

newtype Const a b = Const a

runConst :: forall a b. Const a b -> a
runConst (Const a) = a

instance functorConst :: Functor (Const a) where
  map _ (Const a) = Const a

instance applyConst :: Semigroup a => Apply (Const a) where
  apply (Const a1) (Const a2) = Const (a1 <> a2)

example2 :: Const String Unit
example2 = do
  Const "Do"
  Const " notation"
  Const " for"
  Const " Apply"
  where
  discard x f = x *> f unit

main = do
  log example1
  log $ runConst example2
  log "Done"
