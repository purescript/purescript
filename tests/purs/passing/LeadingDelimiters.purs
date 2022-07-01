module Main
  (
  , main
  ) where

import Prelude
  hiding
  (
  , map
  , unit
  )
import Data.Maybe
  (
  , Maybe(
         , Nothing
         , Just
         )
  )
import Effect.Console
  (
  , log
  )

data X =
  | A

data Y =
  | B
  | C

alpha :: Array X
alpha =
  [
  , A
  ]

bravo ::
  {
  , foo :: Y
  }
bravo = 
  {
  , foo: B
  }

charlie :: Array X -> Int
charlie
  [
  , A
  ]
  = 1
charlie _ = 0

delta
  :: Record (
            , foo :: Y
            )
  -> Y
delta
  {
  , foo
  }
  = foo

setBarToC
  :: {
     , foo :: Y
     | ()
     }
  -> {
     , foo :: Y
     }
setBarToC x =
  x {
    , foo = C
    }

class
  (
  , Functor f
  , Functor g
  ) <= F2 f g | 
              , f -> g
              , g -> f

main = log "Done"
