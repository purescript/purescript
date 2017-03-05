-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude
import Data.Record (class RowUnion)
import Unsafe.Coerce (unsafeCoerce)

type Mand r = (title :: String, flag :: Boolean | r)
type Opt = (optional :: Int)

withDefaults :: forall o. RowUnion (Mand o) Opt (Mand Opt) => {|Mand o} -> {|Mand Opt}
withDefaults = unsafeCoerce

main = withDefaults {title:"Title", flag: true, others:""}
