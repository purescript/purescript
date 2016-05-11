-- @shouldFailWith MultipleValueOpFixities
module MultipleValueOpFixities where

import Prelude

add x y = x + y

infix 2 add as !?
infix 2 add as !?
