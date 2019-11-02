module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Newtype (class Newtype, unwrap)

class TypeEquals a b | a -> b, b -> a where
  coerce :: a -> b
  coerceBack :: b -> a

instance refl :: TypeEquals a a where
  coerce = identity
  coerceBack = identity

newtype RecordNewtype = RecordNewtype { x :: String }

instance newtypeRecordNewtype ::
  TypeEquals inner { x :: String }
    => Newtype RecordNewtype inner where
  wrap = RecordNewtype <<< coerce
  unwrap (RecordNewtype rec) = coerceBack rec

main :: Effect Unit
main = log (unwrap (RecordNewtype { x: "Done" })).x
