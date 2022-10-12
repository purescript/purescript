-- @shouldWarnWith WarningParsingModule
module DeprecatedCaseOfOffsideSyntax1 where

data Foo = Foo Int

test :: Foo -> Int
test = case _ of
    Foo i ->
  i
