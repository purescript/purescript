module Main where

import Prelude
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert')

patternSimple :: Boolean
patternSimple =
  let x = 25252
  in
   x == 25252

patternDoSimple :: forall e. Eff e Boolean
patternDoSimple = do
  let x = 25252
  pure $ x == 25252

newtype X = X Int

patternNewtype :: Boolean
patternNewtype =
  let X a = X 123
  in
   a == 123

patternDoNewtype :: forall e. Eff e Boolean
patternDoNewtype = do
  let X a = X 123
  pure $ a == 123

data Y = Y Int String Boolean

patternData :: Boolean
patternData =
  let Y a b c = Y 456 "hello, world" false
  in
   a == 456 && b == "hello, world" && not c

patternDataIgnored :: Boolean
patternDataIgnored =
  let Y _ x _ = Y 789 "world, hello" true
  in
   x == "world, hello"

patternDoData :: forall e. Eff e Boolean
patternDoData = do
  let Y a b c = Y 456 "hello, world" false
  pure $ a == 456 && b == "hello, world" && not c

patternDoDataIgnored :: forall e. Eff e Boolean
patternDoDataIgnored = do
  let Y _ x _ = Y 789 "world, hello" true
  pure $ x == "world, hello"

patternArray :: Boolean
patternArray = unsafePartial $
  let [a, b] = [1, 2]
  in
   a == 1 && b == 2

patternDoArray :: forall e. Eff e Boolean
patternDoArray = unsafePartial do
  let [a, b] = [1, 2]
  pure $ a == 1 && b == 2

patternMultiple :: Boolean
patternMultiple = unsafePartial $
  let
    x = 25252
    X a = X x
    Y b c d = Y x "hello, world" false
    Y _ e _ = Y 789 "world, hello" true
    [f, g] = [1, 2]
  in
   x == 25252 && a == 25252 && b == 25252 && c == "hello, world" &&
     not d && e == "world, hello" && f == 1 && g == 2

patternDoMultiple :: forall e. Eff e Boolean
patternDoMultiple = unsafePartial do
  let
    x = 25252
    X a = X x
    Y b c d = Y x "hello, world" false
    Y _ e _ = Y 789 "world, hello" true
    [f, g] = [1, 2]
  pure $ x == 25252 && a == 25252 && b == 25252 && c == "hello, world" &&
     not d && e == "world, hello" && f == 1 && g == 2

patternMultipleWithNormal :: Boolean
patternMultipleWithNormal = unsafePartial $
  let
    x = 25252
    X a = X x
    y = 2525
    Y b c d = Y y "hello, world" false
  in
   x == 25252 && y == 2525 &&
     a == 25252 && b == 2525 && c == "hello, world" && not d

patternDoMultipleWithNormal :: forall e. Eff e Boolean
patternDoMultipleWithNormal = unsafePartial do
  let
    x = 25252
    X a = X x
    y = 2525
    Y b c d = Y y "hello, world" false
  pure $ x == 25252 && y == 2525 &&
    a == 25252 && b == 2525 && c == "hello, world" && not d

patternWithParens :: Boolean
patternWithParens = unsafePartial $
  let
    (x) = 25252
    (X a) = X x
    (Y b c d) = Y x "hello, world" false
    (Y _ e _) = Y 789 "world, hello" true
    ([f, g]) = [1, 2]
  in
   x == 25252 && a == 25252 && b == 25252 && c == "hello, world" &&
     not d && e == "world, hello" && f == 1 && g == 2

patternDoWithParens :: forall e. Eff e Boolean
patternDoWithParens = unsafePartial do
  let
    (x) = 25252
    (X a) = X x
    (Y b c d) = Y x "hello, world" false
    (Y _ e _) = Y 789 "world, hello" true
    ([f, g]) = [1, 2]
  pure $ x == 25252 && a == 25252 && b == 25252 && c == "hello, world" &&
     not d && e == "world, hello" && f == 1 && g == 2

patternWithNamedBinder :: Boolean
patternWithNamedBinder = unsafePartial $
  let
    a@{x, y} = {x: 10, y: 20}
  in
   a.x == 10 && x == 10 && a.y == 20 && y == 20

patternDoWithNamedBinder :: forall e. Eff e Boolean
patternDoWithNamedBinder = unsafePartial do
  let
    a@{x, y} = {x: 10, y: 20}
  pure $
    a.x == 10 && x == 10 && a.y == 20 && y == 20

data List a = Nil | Cons a (List a)
infixr 6 Cons as :

instance eqList :: Eq a => Eq (List a) where
  eq xs ys = go xs ys true
    where
      go _ _ false = false
      go Nil Nil acc = acc
      go (x : xs') (y : ys') acc = go xs' ys' $ acc && (y == x)
      go _ _ _ = false

patternWithInfixOp :: Boolean
patternWithInfixOp = unsafePartial $
  let
    x : xs = 1 : 2 : 3 : 4 : Nil
  in
   x == 1 && xs == 2 : 3 : 4 : Nil

patternDoWithInfixOp :: forall e. Eff e Boolean
patternDoWithInfixOp = unsafePartial do
  let
    x : xs = 1 : 2 : 3 : 4 : Nil
  pure $
    x == 1 && xs == 2 : 3 : 4 : Nil

main :: Eff (assert :: ASSERT, console :: CONSOLE) Unit
main = do
  assert' "simple variable pattern" patternSimple
  assert' "simple variable pattern with do" =<< patternDoSimple
  assert' "constructor pattern (newtype)" patternNewtype
  assert' "constructor pattern (newtype) with do" =<< patternDoNewtype
  assert' "constructor pattern (data)" patternData
  assert' "constructor pattern with ignorances" patternDataIgnored
  assert' "constructor pattern (data) with do" =<< patternDoData
  assert' "constructor pattern with ignorances and do" =<< patternDoDataIgnored
  assert' "array pattern" patternArray
  assert' "array pattern with do" =<< patternDoArray
  assert' "multiple patterns" patternMultiple
  assert' "multiple patterns with do" =<< patternDoMultiple
  assert' "multiple patterns with normal let's" patternMultipleWithNormal
  assert' "multiple patterns with normal let's and do" =<< patternDoMultipleWithNormal
  assert' "multiple patterns with parens" patternWithParens
  assert' "multiple patterns with parens and do" =<< patternDoWithParens
  assert' "multiple patterns with named binder" patternWithNamedBinder
  assert' "multiple patterns with named binder and do" =<< patternDoWithNamedBinder
  assert' "pattern with infix operator" patternWithInfixOp
  assert' "pattern with infix operator and do" =<< patternDoWithInfixOp
  log "Done"
