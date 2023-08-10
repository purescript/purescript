-- @shouldFailWith TypesDoNotUnify
module MissingRecordField where

import Prelude ((>), (&&))

john = { first: "John", last: "Smith", mortal: true }

isOver50 p = p.mortal && p.age > 50.0

result = isOver50 john
