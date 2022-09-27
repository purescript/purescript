module Main where

import Prelude

import Effect.Console (log)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable, traverse, sequence)
import Test.Assert

-- Traverse order is done in alphabetical ordering of labels,
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
  | M2 Int
  | M3 (f a)
  | M4 (RecordFields f a)
  | M5 { nested :: RecordFields f a }
  | M6 Int a (Array Int) (Array a) (f a) (f Int) (RecordFields f a) { nested :: RecordFields f a }
  | M7 (f (f { nested :: RecordFields f a }))

-- Note: all 4 of these constraints are needed to compile this code
derive instance
  ( Eq1 f
  , Eq (f (f { nested :: RecordFields f a }))
  , Eq (f { nested :: RecordFields f a })
  , Eq a
  ) => Eq (M f a)
derive instance Functor f => Functor (M f)
derive instance Foldable f => Foldable (M f)
derive instance Traversable f => Traversable (M f)

type MArrStr = M Array String

traverseStr :: forall f. Traversable f => f String -> Array (f String)
traverseStr = traverse pure

sequenceStr :: forall f. Traversable f => f (Array String) -> Array (f String)
sequenceStr = sequence

m0 = M0 :: MArrStr
m1 = M1 "a" ["b", "c"] :: MArrStr
m2 = M2 0 :: MArrStr
m3 = M3 ["a", "b", "c"] :: MArrStr
m4 = M4 recordValue :: MArrStr
m5 = M5 { nested: recordValue } :: MArrStr
m6 = M6 1 "a" [] ["b"] ["c"] [] recordValue { nested: recordValue } :: MArrStr
m7 = M7 [ [ { nested: recordValue } ] ] :: MArrStr

recordValue :: RecordFields Array String
recordValue =
  { a: "a"
  , zArrayA: ["c"]
  , fa: ["b"]
  , ignore: 1
  , arrayIgnore: [2, 3]
  , fIgnore: [4]
  }

type MArrArrStr = M Array (Array String)

m0' = M0 :: MArrArrStr
m1' = M1 ["a"] [["b"], ["c"]] :: MArrArrStr
m2' = M2 0 :: MArrArrStr
m3' = M3 [["a"], ["b"], ["c"]] :: MArrArrStr
m4' = M4 recordValue' :: MArrArrStr
m5' = M5 { nested: recordValue' } :: MArrArrStr
m6' = M6 1 ["a"] [] [["b"]] [["c"]] [] recordValue' { nested: recordValue' } :: MArrArrStr
m7' = M7 [ [ { nested: recordValue' } ] ] :: MArrArrStr

recordValue' :: RecordFields Array (Array String)
recordValue' =
  { a: ["a"]
  , zArrayA: [["c"]]
  , fa: [["b"]]
  , ignore: 1
  , arrayIgnore: [2, 3]
  , fIgnore: [4]
  }

main = do
  assert' "traverse - m0" $ traverseStr m0 == [m0]
  assert' "traverse - m1" $ traverseStr m1 == [m1]
  assert' "traverse - m2" $ traverseStr m2 == [m2]
  assert' "traverse - m3" $ traverseStr m3 == [m3]
  assert' "traverse - m4" $ traverseStr m4 == [m4]
  assert' "traverse - m5" $ traverseStr m5 == [m5]
  assert' "traverse - m6" $ traverseStr m6 == [m6]
  assert' "traverse - m7" $ traverseStr m7 == [m7]

  assert' "sequence - m0" $ sequenceStr m0' == [m0]
  assert' "sequence - m1" $ sequenceStr m1' == [m1]
  assert' "sequence - m2" $ sequenceStr m2' == [m2]
  assert' "sequence - m3" $ sequenceStr m3' == [m3]
  assert' "sequence - m4" $ sequenceStr m4' == [m4]
  assert' "sequence - m5" $ sequenceStr m5' == [m5]
  assert' "sequence - m6" $ sequenceStr m6' == [m6]
  assert' "sequence - m7" $ sequenceStr m7' == [m7]

  log "Done"
