-- @shouldFailWith TypesDoNotUnify
module NestedRecordLabelOnTypeError where

record :: { a :: Int }
record = { a: "a" } -- Triggers an error, but the label is explicitly not added since it caused other errors to be worse. See https://github.com/purescript/purescript/pull/4411 for more information.
