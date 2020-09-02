-- see #3624
module TypeSynonymInstance where

import Data.Newtype (class Newtype)
import TypeSynonym (MyInt)

newtype MyNT = MyNT MyInt

derive instance ntMyNT :: Newtype MyNT _

foo = 0
