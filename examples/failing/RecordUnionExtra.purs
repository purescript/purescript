-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude
import Data.Record (merge)

type Mand r = (title :: String, flag :: Boolean | r)
type Opt = (optional :: Int)

withDefaults :: forall o. Normalised (Mand (optional :: Int|o)) (Mand Opt) => {|Mand o} -> {|Mand Opt}
withDefaults = merge {optional:1}

main = withDefaults {title:"Title", flag: true, others:""}
