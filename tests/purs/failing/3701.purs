-- @shouldFailWith TypesDoNotUnify
module Main where

import Prim.Row as Row

merge
  :: forall r1 r2 r3 r4
   . Row.Union r1 r2 r3
  => Row.Nub r3 r4
  => Record r1
  -> Record r2
  -> Record r4
merge r = merge r


type FooRow r =
  ( thing1 :: String
  , thing2 :: Int
  | r
  )

type AddedRow =
  ( thing3 :: String )

type AddedRow2 =
  ( thing1 :: String )

fooMerge :: forall addedRow.
  Row.Union addedRow (FooRow ()) (FooRow addedRow) =>
  Row.Nub (FooRow addedRow) (FooRow addedRow) =>
  Record addedRow ->
  Record (FooRow addedRow)
fooMerge addedRow = merge addedRow {thing1: "foo", thing2: 1}

foo1 :: Record (FooRow (AddedRow))
foo1 = fooMerge { thing3: "foo" }

foo2 :: Record (FooRow (AddedRow2))
foo2 = fooMerge { thing1: "foo" }
