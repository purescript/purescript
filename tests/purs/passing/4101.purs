module Main where

import Effect.Console (log)

import Lib

class ClassA :: Type -> Type -> Constraint
class ClassA t a

class ClassB :: Type -> Type -> Constraint
class ClassA t a <= ClassB t a

data VariantF :: (Type -> Type) -> Type
data VariantF fs
data Expr

instance a :: ClassA Expr (VariantF UNIT)
instance b :: ClassB Expr (VariantF UNIT)

main = log "Done"
