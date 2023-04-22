-- @shouldFailWith TypesDoNotUnify
module RecordLabelOnTypeError where

a :: { field :: Int }
a = { field: 1 }

b :: { field :: String }
b = a -- this should trigger an error, telling us the `field` tag where the type discrepancy happened
