module Main where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log)

class Singleton x where
  singleton :: String

instance Singleton Int where
  singleton = "int"

instance Singleton String where
  singleton = "string"

singletonWorks :: Effect (Maybe String)
singletonWorks = do
  let
    left = singleton @Int
    right = singleton @String
  pure if left /= right then Nothing else Just "Singleton failed"

class ConflictingIdent :: Type -> Constraint
class ConflictingIdent a where
  -- The `a` in the type below should refer to the `a`
  -- introduced by the `forall`, not the class head.
  conflictingIdent :: forall a. a -> Int

instance ConflictingIdent String where
  conflictingIdent _ = 1

instance ConflictingIdent Int where
  conflictingIdent _ = 2

conflictingIdentWorks :: Effect (Maybe String)
conflictingIdentWorks = do
  pure if (1 == conflictingIdent @String 4) then Nothing else Just "ConflictingIdent failed"

type M :: Type -> Type
type M x = forall a. a -> Int

class ConflictingIdentSynonym :: Type -> Constraint
class ConflictingIdentSynonym a where
  -- The `a` in the type below should refer to the `a`
  -- introduced by the `forall`, not the class head.
  conflictingIdentSynonym :: M a

instance ConflictingIdentSynonym String where
  conflictingIdentSynonym _ = 1

instance ConflictingIdentSynonym Int where
  conflictingIdentSynonym _ = 2

conflictingIdentSynonymWorks :: Effect (Maybe String)
conflictingIdentSynonymWorks = do
  pure if (1 == conflictingIdentSynonym @String 4) then Nothing else Just "ConflictingIdentSynonym failed"

class MultiNoFDs a b where
  multiNoFds :: Int

instance MultiNoFDs Int Int where
  multiNoFds = 0

instance MultiNoFDs String Int where
  multiNoFds = 1

multiNoFdsWorks :: Effect (Maybe String)
multiNoFdsWorks = do
  let
    left = multiNoFds @Int @Int
    right = multiNoFds @String @Int
  pure if left /= right then Nothing else Just "MultiNoFDs failed"

class MultiWithFDs a b | a -> b where
  multiWithFDs :: Int

instance MultiWithFDs Int Int where
  multiWithFDs = 0

instance MultiWithFDs String Int where
  multiWithFDs = 1

multiWithFdsWorks :: Effect (Maybe String)
multiWithFdsWorks = do
  let
    left = multiWithFDs @Int
    right = multiWithFDs @String
  pure if left /= right then Nothing else Just "MultiWithFds failed"

class MultiWithBidiFDs a b | a -> b, b -> a where
  multiWithBidiFDs :: Int

instance MultiWithBidiFDs Int Int where
  multiWithBidiFDs = 0

instance MultiWithBidiFDs String String where
  multiWithBidiFDs = 1

multiWithBidiFDsLeftWorks :: Effect (Maybe String)
multiWithBidiFDsLeftWorks = do
  let
    left = multiWithBidiFDs @Int
    right = multiWithBidiFDs @String
  pure if left /= right then Nothing else Just "MultiWithFds failed"

multiWithBidiFDsRightWorks :: Effect (Maybe String)
multiWithBidiFDsRightWorks = do
  let
    left = multiWithBidiFDs @_ @Int
    right = multiWithBidiFDs @_ @String
  pure if left /= right then Nothing else Just "MultiWithFds failed"

class Superclass a where
  superClassValue :: a

class Superclass a <= MainClass a where
  mainClassInt :: Int

data A2 = A2

derive instance Eq A2

instance Superclass A2 where
  superClassValue = A2

instance MainClass A2 where
  mainClassInt = 0

data B2 = B2

derive instance Eq B2

instance Superclass B2 where
  superClassValue = B2

instance MainClass B2 where
  mainClassInt = 3

mainClassWorks :: Effect (Maybe String)
mainClassWorks = do
  let
    test1 = 0 == mainClassInt @A2
    test2 = A2 == superClassValue @A2
  pure if test1 && test2 then Nothing else Just "MainClass failed"

class MultiCoveringSets a b c d e f | a b -> c d e f, f e -> a b c d where
  noneOfSets :: Int

  partialOfABSet :: a -> { c :: c, d :: d }

  partialOfFESet :: f -> { c :: c, d :: d }

instance MultiCoveringSets Boolean Boolean String String Int Int where
  noneOfSets = 1
  partialOfABSet a = { c: if a then "101" else "100", d: "1" }
  partialOfFESet f = { c: show f, d: "1" }

instance MultiCoveringSets Int Int String String Boolean Boolean where
  noneOfSets = 2
  partialOfABSet a = { c: show a, d: "2" }
  partialOfFESet f = { c: show f, d: "2" }

multiCoveringSetsWorks :: Effect (Maybe String)
multiCoveringSetsWorks = do
  let
    test1a = 1 == noneOfSets @Boolean @Boolean
    test1b = "101" == (partialOfABSet @Boolean @Boolean true).c
    test1c = show 3 == (partialOfFESet @_ @_ @_ @_ @Int @Int 3).c
    test2a = 2 == noneOfSets @_ @_ @_ @_ @Boolean @Boolean
    test2b = show 20 == (partialOfABSet @_ @_ @_ @_ @Boolean @Boolean 20).c
    test2c = show false == (partialOfFESet @_ @_ @_ @_ @Boolean @Boolean false).c
    passes = test1a && test1b && test1c && test2a && test2b && test2c
  pure if passes then Nothing else Just "MultiCoveringSets failed"

main = do
  arr' <- sequence
    [ singletonWorks
    , conflictingIdentWorks
    , conflictingIdentSynonymWorks
    , multiNoFdsWorks
    , multiWithFdsWorks
    , multiWithBidiFDsLeftWorks
    , multiWithBidiFDsRightWorks
    , mainClassWorks
    ]
  case NEA.fromArray $ Array.catMaybes arr' of
    Just errs ->
      log $ "Errors..." <> (Array.intercalate "\n" $ NEA.toArray errs)
    Nothing ->
      log "Done"
