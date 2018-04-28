-- @shouldFailWith NonAssociativeError
module Main where

infix 6 type Function as >>

const :: forall a b. a >> b >> a
const a _ = a
