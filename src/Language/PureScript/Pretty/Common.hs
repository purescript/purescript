-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module Language.PureScript.Pretty.Common where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (nub, intersperse, intercalate)
import Data.Function (fix)
import Control.Monad.State
import Control.Applicative (Applicative(..), Alternative(..))
import qualified Control.Category as C
import Control.Category ((>>>))
import qualified Control.Arrow as A
import Control.Arrow ((***), (<+>))

import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Declarations

identToJs :: Ident -> String
identToJs (Ident name) = name
identToJs (Op op) = concatMap opCharToString op
  where
  opCharToString :: Char -> String
  opCharToString = (:) '$'. show . ord

newtype Pattern a b = Pattern { runPattern :: A.Kleisli Maybe a b } deriving (C.Category, A.Arrow, A.ArrowZero, A.ArrowPlus)

instance Functor (Pattern a) where
  fmap f (Pattern p) = Pattern $ A.Kleisli $ fmap f . A.runKleisli p

pattern :: Pattern a b -> a -> Maybe b
pattern = A.runKleisli . runPattern

parens :: String -> String
parens s = ('(':s) ++ ")"

chainl :: Pattern a (a, a) -> (r -> r -> r) -> Pattern a r -> Pattern a r
chainl split f p = fix $ \c -> split >>> ((c <+> p) *** p) >>> A.arr (uncurry f)

chainr :: Pattern a (a, a) -> (r -> r -> r) -> Pattern a r -> Pattern a r
chainr split f p = fix $ \c -> split >>> (p *** (c <+> p)) >>> A.arr (uncurry f)

wrap :: Pattern a (s, a) -> (s -> r -> r) -> Pattern a r -> Pattern a r
wrap split f p = fix $ \c -> split >>> (C.id *** (c <+> p)) >>> A.arr (uncurry f)

split :: Pattern a (s, t) -> (s -> t -> r) -> Pattern a r -> Pattern a r
split s f p = s >>> A.arr (uncurry f)

data OperatorTable a r = OperatorTable { runOperatorTable :: [ [Operator a r] ] }

data Operator a r where
  AssocL :: Pattern a (a, a) -> (r -> r -> r) -> Operator a r
  AssocR :: Pattern a (a, a) -> (r -> r -> r) -> Operator a r
  Wrap   :: Pattern a (s, a) -> (s -> r -> r) -> Operator a r
  Split  :: Pattern a (s, t) -> (s -> t -> r) -> Operator a r

buildPrettyPrinter :: OperatorTable a r -> Pattern a r -> Pattern a r
buildPrettyPrinter table p = foldl (\p' ops -> foldl1 (<+>) (flip map ops $ \op ->
  case op of
    AssocL pat g -> chainl pat g p'
    AssocR pat g -> chainr pat g p'
    Wrap pat g -> wrap pat g p'
    Split pat g -> split pat g p'
  ) <+> p') p $ runOperatorTable table
