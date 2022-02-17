module Main where

import Prelude

import Effect.Console (log)

data Tuple a b = Tuple a b

tuple :: forall @a @b. a -> b -> Tuple a b
tuple = Tuple

tuple' :: Tuple Int Int
tuple' = tuple @Int @Int 21 42

data Id a = Id a

id :: forall @a. a -> Id a
id = Id

id' :: Id (forall a. a -> a)
id' = id @(forall a. a -> a) (\x -> x)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

class TypeClass1 a

instance TypeClass1 Int

tc1 :: forall @a. TypeClass1 a => Proxy a
tc1 = Proxy

tc1' :: Proxy Int
tc1' = tc1 @Int

class TypeClass2 a b | a -> b

instance TypeClass2 Int String

tc2 :: forall @a b. TypeClass2 a b => Proxy ( a :: a, b :: b )
tc2 = Proxy

tc2' :: Proxy ( a :: Int, b :: String )
tc2' = tc2 @Int

class TypeClass3 @f where
  tc3 :: Proxy ( f :: f )

instance TypeClass3 Id where
  tc3 = Proxy

tc3' :: Proxy ( f :: Id )
tc3' = tc3 @Id

main = log "Done"
