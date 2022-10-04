-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FoldableInstance9 where

import Prelude
import Data.Tuple (Tuple)
import Data.Foldable (class Foldable)

type Rec f a =
  { all :: f a a a
  , rights :: f Int a a
  , lefts :: f a a Int
  , middle :: f Int a Int
  , none :: f Int Int Int
  }
data Test f g h a
  = Test1 (f a a a) (f Int a a) (f a a Int) (f Int a Int) (f Int Int Int)
  | Test2 { all :: f a a a
          , rights :: f Int a a
          , lefts :: f a a Int
          , middle :: f Int a Int
          , none :: f Int Int Int
          }
  | Test3 (g
            { all :: f a a a
            , rights :: f Int a a
            , lefts :: f a a Int
            , middle :: f Int a Int
            , none :: f Int Int Int
            }
            a)
  | Test4 (h
            { nested1 ::
                { all :: f a a a
                , rights :: f Int a a
                , lefts :: f a a Int
                , middle :: f Int a Int
                , none :: f Int Int Int
                }
            , nested2 ::
                g
                  { all :: f a a a
                  , rights :: f Int a a
                  , lefts :: f a a Int
                  , middle :: f Int a Int
                  , none :: f Int Int Int
                  }
                  a
            }
            a)
  | Test5 (Rec f a)
  | Test6 (g (Rec f a) a)
  | Test7 (h { nested1 :: Rec f a, nested2 :: g (Rec f a) a } a)
derive instance Foldable (Test f g h)
