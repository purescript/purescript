module SourceMaps.Leakage where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))

import Data.Int as Int

data Dtor = Dtor Int Int
newtype Ntor = Ntor (Tuple Int Int)
type IntType = Int


foo :: String -> String -> String -> String -> String
foo s1 s2 sUnused s4 = fromMaybe "" do
  i <- Int.fromString s1
  whole@{ p } <- pure { p: 1, h: 2 }
  let
    mutuallyRecursive1 k b
      | k <= 0 = b
      | otherwise = mutuallyRecursive2 (k - 1) b
      
    mutuallyRecursive2 k b
      | k <= 0 = b
      | otherwise = mutuallyRecursive1 (k - 1) b
      
    -- s4 here shadows s4 arg above
    patternGuards s4
      | s4 == "a string" = s4
      
      | s4 == "another string"
      , s4 /= "" = s4
      
      | Just anotherInt <- Int.fromString s4
      , anotherInt `mod` 2 == 0 = s4
      
      | otherwise = s4
      
    caseOfInt = case _ of
      1 -> 1
      _ -> 2
      
    caseOfNum = case _ of
      1.0 -> 1
      _ -> 2
      
    caseOfNegInt = case _ of
      (-1) -> 1
      _ -> 2
      
    caseOfNegNum = case _ of
      (-1.0) -> 1
      _ -> 2
      
    caseOfString = case _ of
      "1" -> 1
      _ -> 2
      
    caseOfChar = case _ of
      '1' -> 1
      _ -> 2
      
    caseOfBool = case _ of
      true -> 1
      false -> 2
      
    caseOfArr = case _ of
      [] -> 1
      ["foo"] -> 2
      _ -> 3
      
    caseOfRec = case _ of
      { alpha, beta: 8 } -> alpha
      { beta: 1 } -> 2
      _ -> 3
      
    caseOfDtor = case _ of
      Dtor 1 _ -> 1
      Dtor x@1 2 -> x
      Dtor _ y -> 2 + y
      
    caseOfNtor = case _ of
      Ntor (Tuple 1 _) -> 1
      Ntor (Tuple x@1 2) -> x
      Ntor (Tuple _ y) -> 2 + y

    casePartial :: Partial => Dtor -> Int
    casePartial a = case a of
      Dtor 1 _ -> 1
      
  _ <- pure s4
  res <- bar 4 (pure [1, 2, 3])
  let
    extra = show $ map show
      [ caseOfInt 4
      , caseOfNegInt (-4)
      , caseOfNum 4.0
      , caseOfNegNum (-4.0)
      , caseOfString "afadf"
      , caseOfChar 'x'
      , caseOfBool true
      , caseOfArr []
      , caseOfRec { alpha: 0, beta: 4 }
      , caseOfDtor $ Dtor 8 10
      , caseOfNtor $ Ntor $ Tuple 1 8
      ]
      
    foo = "shadowed name"
    unusedBinding = "unused"
  pure $ show i <> patternGuards "abc" <> extra <> foo <> show res

  where
  bar :: forall c f a. Show a => Functor f => Monad c => IntType -> c (f a) -> c (f String)
  bar = baz
    where
    baz i c = do 
      f <- c
      pure $ map (\a -> show a <> " " <> show i) f

fact :: Int -> Int
fact n = go n
  where
    go n = if n == 0 then 1 else n * go (n - 1)

factTCO :: Int -> Int
factTCO n = go n 1
  where
    go n a = if n == 0 then a else go (n - 1) (n * a)

comp :: Int -> Maybe Int
comp = (pure <<< (+) 1) <=< Just <<< fact <<< factTCO

nestedIfs :: Int -> Int
nestedIfs v = 
  if v > 0 && v /= 0 then
    if v < 0 then 1
    else 2
  else 3

class MultiDimensionalDrift :: Type -> Symbol -> Row Type -> Int -> Constraint
class MultiDimensionalDrift ty sym row int | ty sym -> row int where
  example :: ty -> Proxy sym -> { | row } -> Proxy int

instance MultiDimensionalDrift Int "Int" row 0 where
  example _ _ _ = Proxy

else instance MultiDimensionalDrift Number "Int" row 0 where
  example b1 b2 b3 = Proxy