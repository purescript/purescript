-- @shouldFailWith PropertyIsMissing
-- TODO: Make this fail with a new error ExtraProperty instead.
module ExtraRecordField where

import Prelude ((<>))

full :: { first :: String, last :: String } -> String
full p = p.first <> " " <> p.last

oops = full { first: "Jane", last: "Smith", age: 29 }
