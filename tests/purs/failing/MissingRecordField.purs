-- @shouldFailWith PropertyIsMissing
module MissingRecordField where

import Prelude ((>))

john = { first: "John", last: "Smith" }

isOver50 p = p.age > 50.0

result = isOver50 john
