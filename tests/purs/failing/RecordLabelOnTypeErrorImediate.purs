-- @shouldFailWith TypesDoNotUnify
module NestedRecordLabelOnTypeError where

record :: { a :: Int }
record = { a: "a" } -- this should trigger an error, telling us there's a mismatch in the field `a`, but it doesn't
