-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Kinds
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

module Language.PureScript.Pretty.Kinds (
    prettyPrintKind
) where

import Data.Maybe (fromMaybe)

import Language.PureScript.Kinds
import Language.PureScript.Pretty.Common
import Language.PureScript.Unknown
import Control.Arrow (ArrowPlus(..))

typeLiterals :: Pattern () Kind String
typeLiterals = mkPattern match
  where
  match Star = Just "*"
  match Row = Just "#"
  match (KUnknown (Unknown u)) = Just $ 'u' : show u
  match _ = Nothing

funKind :: Pattern () Kind (Kind, Kind)
funKind = mkPattern match
  where
  match (FunKind arg ret) = Just (arg, ret)
  match _ = Nothing

prettyPrintKind :: Kind -> String
prettyPrintKind = fromMaybe (error "Incomplete pattern") . pattern matchKind ()
  where
  matchKind :: Pattern () Kind String
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchKind)
  operators :: OperatorTable () Kind String
  operators =
    OperatorTable [ [ AssocR funKind $ \arg ret -> arg ++ " -> " ++ ret ] ]
