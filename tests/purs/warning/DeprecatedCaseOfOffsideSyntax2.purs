-- @shouldWarnWith WarningParsingModule
module DeprecatedCaseOfOffsideSyntax2 where

data Foo = Foo Int

test :: Foo -> Int
test = case _ of Foo i ->
  i
