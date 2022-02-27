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

kinded :: forall @k (@a :: k) j (@b :: j). Proxy (a :: Proxy a, k :: Proxy k, b :: Proxy b, j :: Proxy j)
kinded = Proxy

kinded' :: Proxy ( a :: Proxy "Int", b :: Proxy "Int", j :: Proxy Symbol, k :: Proxy Symbol )
kinded' = kinded @Symbol @"Int" @"Int"

changeId :: forall a. a -> a
changeId x = x

changeId' :: Int -> Int
changeId' = (changeId :: forall @a. a -> a) @Int

data Either @a @b = Left a | Right b

left :: Either Int String
left = Left @Int @String 0

right :: Either Int String
right = Right @Int @String "0"

main = log "Done"
