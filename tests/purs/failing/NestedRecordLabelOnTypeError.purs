-- @shouldFailWith TypesDoNotUnify
module NestedRecordLabelOnTypeError where

record :: { a :: { b :: { c :: Int } } }
record = { a: { b: { c: 1 } } }

error :: { a :: { b :: { c :: String } } }
error = record -- this should trigger an error, telling us there's a mismatch in the field `a > b > c`
