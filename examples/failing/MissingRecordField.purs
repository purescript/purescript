-- @shouldFailWith TypesDoNotUnify
-- TODO: Update type checker to make this fail with PropertyIsMissing instead. 
module MissingRecordField where

import Prelude ((>))

john = { first: "John", last: "Smith" }

isOver50 p = p.age > 50.0

result = isOver50 john
