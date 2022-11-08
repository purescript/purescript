-- @shouldFailWith TypesDoNotUnify
module NestedRecordLabelOnTypeError where

record :: { field :: Int }
record = { field: "a" } -- this should trigger an error, telling us there's a mismatch in the field `a`.
