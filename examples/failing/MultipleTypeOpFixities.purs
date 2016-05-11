-- @shouldFailWith MultipleTypeOpFixities
module MultipleTypeOpFixities where

import Prelude

type Op x y = Op x y

infix 2 type Op as !?
infix 2 type Op as !?
