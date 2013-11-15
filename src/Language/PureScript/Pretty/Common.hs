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

newtype Pattern u a b = Pattern { runPattern :: A.Kleisli (StateT u Maybe) a b } deriving (C.Category, A.Arrow, A.ArrowZero, A.ArrowPlus)

instance Functor (Pattern u a) where
  fmap f (Pattern p) = Pattern $ A.Kleisli $ fmap f . A.runKleisli p

pattern :: Pattern u a b -> u -> a -> Maybe b
pattern p u = flip evalStateT u . A.runKleisli (runPattern p)

mkPattern :: (a -> Maybe b) -> Pattern u a b
mkPattern f = Pattern $ A.Kleisli (lift . f)

mkPattern' :: (a -> StateT u Maybe b) -> Pattern u a b
mkPattern' = Pattern . A.Kleisli

parens :: String -> String
parens s = ('(':s) ++ ")"

chainl :: Pattern u a (a, a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainl split f p = fix $ \c -> split >>> ((c <+> p) *** p) >>> A.arr (uncurry f)

chainr :: Pattern u a (a, a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainr split f p = fix $ \c -> split >>> (p *** (c <+> p)) >>> A.arr (uncurry f)

wrap :: Pattern u a (s, a) -> (s -> r -> r) -> Pattern u a r -> Pattern u a r
wrap split f p = fix $ \c -> split >>> (C.id *** (c <+> p)) >>> A.arr (uncurry f)

split :: Pattern u a (s, t) -> (s -> t -> r) -> Pattern u a r -> Pattern u a r
split s f p = s >>> A.arr (uncurry f)

data OperatorTable u a r = OperatorTable { runOperatorTable :: [ [Operator u a r] ] }

data Operator u a r where
  AssocL :: Pattern u a (a, a) -> (r -> r -> r) -> Operator u a r
  AssocR :: Pattern u a (a, a) -> (r -> r -> r) -> Operator u a r
  Wrap   :: Pattern u a (s, a) -> (s -> r -> r) -> Operator u a r
  Split  :: Pattern u a (s, t) -> (s -> t -> r) -> Operator u a r

buildPrettyPrinter :: OperatorTable u a r -> Pattern u a r -> Pattern u a r
buildPrettyPrinter table p = foldl (\p' ops -> foldl1 (<+>) (flip map ops $ \op ->
  case op of
    AssocL pat g -> chainl pat g p'
    AssocR pat g -> chainr pat g p'
    Wrap pat g -> wrap pat g p'
    Split pat g -> split pat g p'
  ) <+> p') p $ runOperatorTable table
