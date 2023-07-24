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

instance Singleton Int where singleton = "int"
instance Singleton String where singleton = "string"

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

conflictingIdentSynonymWorks :: Effect (Maybe String)
conflictingIdentSynonymWorks = do
  pure if (1 == conflictingIdentSynonym @String 4) then Nothing else Just "ConflictingIdentSynonym failed"

class MultiNoFDs a b where
  multiNoFds :: Int

instance MultiNoFDs Int Int where multiNoFds = 0
instance MultiNoFDs String Int where multiNoFds = 1

-- multiNoFdsWorks :: Effect (Maybe String)
-- multiNoFdsWorks = do
--   let
--     {-
--      No type class instance was found for
                              
--            Main.MultiNoFDs Int
--                            t1 
                              
--          The instance head contains unknown type variables. Consider adding a type annotation.
       
--        while checking that expression left
--     -}
--     left = multiNoFds @Int @Int
--     right = multiNoFds @String @Int
--   pure if left /= right then Nothing else Just "MultiNoFDs failed"

class MultiWithFDs a b | a -> b where
  multiWithFDs :: Int

instance MultiWithFDs Int Int where multiWithFDs = 0
instance MultiWithFDs String Int where multiWithFDs = 1

multiWithFdsWorks :: Effect (Maybe String)
multiWithFdsWorks = do
  let
    left = multiWithFDs @Int
    right = multiWithFDs @String
  pure if left /= right then Nothing else Just "MultiWithFds failed"

class MultiWithBidiFDs a b | a -> b, b -> a where
  multiWithBidiFDs :: Int

instance MultiWithBidiFDs Int Int where multiWithBidiFDs = 0
instance MultiWithBidiFDs String String where multiWithBidiFDs = 1

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

main = do
  arr' <- sequence
    [ singletonWorks
    , conflictingIdentWorks
    , conflictingIdentSynonymWorks
    -- , multiNoFdsWorks
    , multiWithFdsWorks
    , multiWithBidiFDsLeftWorks
    , multiWithBidiFDsRightWorks
    ]
  case NEA.fromArray $ Array.catMaybes arr' of
    Just errs ->
      log $ "Errors..." <> (Array.intercalate "\n" $ NEA.toArray errs)
    Nothing ->
      log "Done"