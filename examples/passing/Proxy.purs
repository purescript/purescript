module Main (main) where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)

f :: @Int -> Unit
f _ = unit

g :: forall eff. @(console :: CONSOLE | eff) -> Unit
g _ = unit

h :: @"foo" -> Unit
h _ = unit

i :: @"foo"
i = @"foo"

j :: Unit
j = h i

data P t = P

switchP :: forall p. @p -> P p
switchP _ = P :: P p

switchP' :: forall p. P p -> @p
switchP' P = @p

type Ap f x = f x
infix 4 type Ap as $
type Eg0 = Array $ Unit
type Eg1 = Array $ Unit

eg0 :: P Eg0
eg0 = switchP @Eg1

eg0' :: @Eg0
eg0' = switchP' (P :: P Eg1)

eg1 :: @Eg0
eg1 = switchP' (switchP @Eg1)

eg1' :: P Eg0
eg1' = switchP (switchP' (P :: P Eg0))


class Go a b | a -> b

instance goInst :: Go Int Int

goGo :: forall a b c. Go a b => Go b c => @a -> P c
goGo _ = P :: P c

go0 :: P Int
go0 = goGo @Int

type Go1 = Int
type Go1' = Int

go1 :: P Go1
go1 = goGo @Go1'


class Determined a p | a -> p where
  determined :: a -> p

instance determinedIntProxy :: Determined Int @Int where
  determined _ = @Int

instance determinedProxyInt :: Determined @Int Int where
  determined _ = 42

determined0 :: @Int
determined0 = determined 42

determined1 :: Int
determined1 = determined @Int


main = log "Done"
