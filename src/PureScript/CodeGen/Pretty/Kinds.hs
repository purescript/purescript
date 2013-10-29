-----------------------------------------------------------------------------
--
-- Module      :  PureScript.CodeGen.Pretty.Kinds
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

module PureScript.CodeGen.Pretty.Kinds (
    prettyPrintKind
) where

import Data.Maybe (fromMaybe)
import Data.List (intersperse, intercalate)
import qualified Control.Arrow as A
import Control.Arrow ((<+>))
import qualified Data.Map as M
import Control.Applicative

import PureScript.Kinds
import PureScript.CodeGen.Common

typeLiterals :: Pattern Kind String
typeLiterals = Pattern $ A.Kleisli match
  where
  match Star = Just "*"
  match Row = Just "ρ"
  match (KUnknown u) = Just $ "u" ++ show u
  match _ = Nothing

funKind :: Pattern Kind (Kind, Kind)
funKind = Pattern $ A.Kleisli match
  where
  match (FunKind arg ret) = Just (arg, ret)
  match _ = Nothing

prettyPrintKind :: Kind -> String
prettyPrintKind = fromMaybe (error "Incomplete pattern") . pattern matchKind
  where
  matchKind :: Pattern Kind String
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchKind)
  operators :: OperatorTable Kind String
  operators =
    OperatorTable $ [ [ AssocR funKind $ \arg ret -> arg ++ " -> " ++ ret ] ]
