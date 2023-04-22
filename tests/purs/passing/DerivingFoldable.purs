module Main where

import Prelude
import Effect.Console (log)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Test.Assert

-- Fold is done in alphabetical ordering of labels,
-- not their order in definition
type RecordFields f a =
  { a :: a
  , zArrayA :: Array a
  , fa :: f a
  , ignore :: Int
  , arrayIgnore :: Array Int
  , fIgnore :: f Int
  }

data M f a
  = M0
  | M1 a (Array a)
  | M2 Int (forall a. Array a -> Array a)
  | M3 (f a)
  | M4 (RecordFields f a)
  | M5 { nested :: RecordFields f a }
  | M6 Int a (Array Int) (Array a) (f a) (f Int) (RecordFields f a) { nested :: RecordFields f a }
  | M7 (f (f { nested :: RecordFields f a }))

derive instance foldableM :: Foldable f => Foldable (M f)

type MArrStr = M Array String

foldlStr :: forall f. Foldable f => f String -> String
foldlStr = foldl (\acc next -> acc <> "<" <> next) "Start"

foldrStr :: forall f. Foldable f => f String -> String
foldrStr = foldr (\next acc -> next <> ">" <> acc) "Start"

foldMapStr :: forall f. Foldable f => f String -> String
foldMapStr = foldMap identity

m0 = M0 :: MArrStr
m1 = M1 "a" ["b", "c"] :: MArrStr
m2 = M2 0 identity :: MArrStr
m3 = M3 ["a", "b", "c"] :: MArrStr
m4 = M4 recordValue :: MArrStr
m5 = M5 { nested: recordValue } :: MArrStr
m6 = M6 1 "a" [] ["b"] ["c"] [] recordValue { nested: recordValue } :: MArrStr
m7 = M7 [[{ nested: recordValue }]] :: MArrStr

recordValue :: RecordFields Array String
recordValue =
  { a: "a"
  , zArrayA: ["c"]
  , fa: ["b"]
  , ignore: 1
  , arrayIgnore: [2, 3]
  , fIgnore: [4]
  }

main = do
  assertEqual' "foldl - M0" { expected: "Start", actual: foldlStr m0 }
  assertEqual' "foldl - M1" { expected: "Start<a<b<c", actual: foldlStr m1 }
  assertEqual' "foldl - M2" { expected: "Start", actual: foldlStr m2 }
  assertEqual' "foldl - M3" { expected: "Start<a<b<c", actual: foldlStr m3 }
  assertEqual' "foldl - M4" { expected: "Start<a<b<c", actual: foldlStr m4 }
  assertEqual' "foldl - M5" { expected: "Start<a<b<c", actual: foldlStr m5 }
  assertEqual' "foldl - M6" { expected: "Start<a<b<c<a<b<c<a<b<c", actual: foldlStr m6 }
  assertEqual' "foldl - M7" { expected: "Start<a<b<c", actual: foldlStr m7 }

  assertEqual' "foldr - M0" { expected: "Start", actual: foldrStr m0 }
  assertEqual' "foldr - M1" { expected: "a>b>c>Start", actual: foldrStr m1 }
  assertEqual' "foldr - M2" { expected: "Start", actual: foldrStr m2 }
  assertEqual' "foldr - M3" { expected: "a>b>c>Start", actual: foldrStr m3 }
  assertEqual' "foldr - M4" { expected: "a>b>c>Start", actual: foldrStr m4 }
  assertEqual' "foldr - M5" { expected: "a>b>c>Start", actual: foldrStr m5 }
  assertEqual' "foldr - M6" { expected: "a>b>c>a>b>c>a>b>c>Start", actual: foldrStr m6 }
  assertEqual' "foldr - M7" { expected: "a>b>c>Start", actual: foldrStr m7 }

  assertEqual' "foldMap - M0" { expected: "", actual: foldMapStr m0 }
  assertEqual' "foldMap - M1" { expected: "abc", actual: foldMapStr m1 }
  assertEqual' "foldMap - M2" { expected: "", actual: foldMapStr m2 }
  assertEqual' "foldMap - M3" { expected: "abc", actual: foldMapStr m3 }
  assertEqual' "foldMap - M4" { expected: "abc", actual: foldMapStr m4 }
  assertEqual' "foldMap - M5" { expected: "abc", actual: foldMapStr m5 }
  assertEqual' "foldMap - M6" { expected: "abcabcabc", actual: foldMapStr m6 }
  assertEqual' "foldMap - M7" { expected: "abc", actual: foldMapStr m7 }

  log "Done"
