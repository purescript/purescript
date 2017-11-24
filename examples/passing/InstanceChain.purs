module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Arg i o | i -> o

data Proxy p = Proxy

arg :: forall i o. Arg i o => Proxy i -> Proxy o
arg _ = Proxy

instance appArg :: Arg i o => Arg (f i) o
else instance reflArg :: Arg i i

argEg0 :: Proxy Int
argEg0 = arg (Proxy :: Proxy Int)

argEg1 :: Proxy Int
argEg1 = arg (Proxy :: Proxy (Array Int))

argEg2 :: Proxy Int
argEg2 = arg (Proxy :: Proxy (Boolean -> Array Int))


class IsEq l r o | l r -> o

foreign import data True :: Type
foreign import data False :: Type

isEq :: forall l r o. IsEq l r o => Proxy l -> Proxy r -> Proxy o
isEq _ _ = Proxy

instance reflIsEq :: IsEq a a True
else instance notIsEq :: IsEq a b False

isEqEg0 :: Proxy True
isEqEg0 = isEq (Proxy :: Proxy Int) (Proxy :: Proxy Int)

isEqEg1 :: Proxy True
isEqEg1 = isEq (Proxy :: Proxy (Array Int)) (Proxy :: Proxy (Array Int))

isEqEg2 :: Proxy False
isEqEg2 = isEq (Proxy :: Proxy (Array Int)) (Proxy :: Proxy (Array Boolean))


-- example chain in which we should only commit to `isStringElse` once we've
-- learnt that the type param is apart from `String`.

class Learn a b | a -> b
instance learnInst :: Learn a a

class IsString t o | t -> o
instance isStringString :: IsString String True
else instance isStringElse :: IsString t False

learnIsString :: forall a t o.
  IsString t o =>
  Learn a t =>
  Proxy a ->
  Proxy o
learnIsString _ = Proxy

isStringEg0 :: Proxy True
isStringEg0 = learnIsString (Proxy :: Proxy String)

isStringEg1 :: Proxy False
isStringEg1 = learnIsString (Proxy :: Proxy Int)


main = log "Done"
