module Main where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Compose (Compose)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Effect.Console (log)
import Test.Assert (assert)

newtype ManyErrors e a = ManyErrors (Either e (Maybe a))

derive instance eqManyErrors :: (Eq e, Eq a) => Eq (ManyErrors e a)

derive via (Compose (Either e) Maybe)
  instance manyErrorsFunctor :: Functor (ManyErrors e)

newtype GenericBounded a = GenericBounded a

derive instance eqGenericBounded :: Eq a => Eq (GenericBounded a)
derive instance ordGenericBounded :: Ord a => Ord (GenericBounded a)
derive instance newtypeGenericBounded :: Newtype (GenericBounded a) _

instance boundedGenericBounded ::
  ( Ord a
  , Generic a rep
  , GenericBottom rep
  , GenericTop rep
  ) => Bounded (GenericBounded a) where
    bottom = wrap genericBottom
    top = wrap genericTop

data ABC = A | B | C

derive instance eqABC :: Eq ABC
derive instance ordABC :: Ord ABC
derive instance genericABC :: Generic ABC _

derive via (GenericBounded ABC) instance boundedABC :: Bounded ABC

data XYZ = X | Y | Z

derive instance eqXYZ :: Eq XYZ
derive instance ordXYZ :: Ord XYZ
derive instance genericXYZ :: Generic XYZ _

type V = GenericBounded XYZ
derive via V instance boundedXYZ :: Bounded XYZ

viaNotReserved :: ∀ a. a -> a
viaNotReserved via = via

data ViaNotReserved via = ViaNotReserved via

type ViaNotReservedInRows a = ( via :: a )

main = do
  assert $ (map (_ + 1) $ ManyErrors (Left unit) :: ManyErrors Unit Int) == (ManyErrors $ Left unit)
  assert $ (map (_ + 1) $ ManyErrors (Right Nothing) :: ManyErrors Unit Int) == (ManyErrors $ Right Nothing)
  assert $ (map (_ + 1) $ ManyErrors (Right $ Just 0) :: ManyErrors Unit Int) == (ManyErrors (Right $ Just 1))

  assert $ bottom == A
  assert $ top == C

  log "Done"
