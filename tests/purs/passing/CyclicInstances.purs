module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Console (log)

newtype A = A B
derive newtype instance Show A
data B = B C
       | Z
derive instance Generic B _
instance Show B where show x = genericShow x
newtype C = C A
derive instance Generic C _
instance Show C where show = genericShow

newtype A2 = A2 { x :: B2 }
derive newtype instance Show A2
data B2 = B2 C2
        | Z2
derive instance Generic B2 _
instance Show B2 where show x = genericShow x
newtype C2 = C2 A2
derive instance Generic C2 _
instance Show C2 where show = genericShow

main = log "Done"
