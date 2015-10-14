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
-- Pretty printer for kinds
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Kinds (
    prettyPrintKind,
    kindAsBox
) where

import Data.Maybe (fromMaybe)

import Control.Arrow (ArrowPlus(..))
import Control.PatternArrows

import Language.PureScript.Kinds

import Text.PrettyPrint.Boxes (Box(), text, render, (<>))

typeLiterals :: Pattern () Kind Box
typeLiterals = mkPattern match
  where
  match Star = Just $ text "*"
  match Bang = Just $ text "!"
  match (KUnknown u) = Just $ text $ 'u' : show u
  match _ = Nothing

matchRow :: Pattern () Kind ((), Kind)
matchRow = mkPattern match
  where
  match (Row k) = Just ((), k)
  match _ = Nothing

funKind :: Pattern () Kind (Kind, Kind)
funKind = mkPattern match
  where
  match (FunKind arg ret) = Just (arg, ret)
  match _ = Nothing

-- | Generate a pretty-printed string representing a Kind
prettyPrintKind :: Kind -> String
prettyPrintKind = render . kindAsBox

kindAsBox :: Kind -> Box
kindAsBox = fromMaybe (error "Incomplete pattern") . pattern matchKind ()
  where
  matchKind :: Pattern () Kind Box
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap ((text "(" <>) . (<> text ")")) matchKind)

  operators :: OperatorTable () Kind Box
  operators =
    OperatorTable [ [ Wrap matchRow $ \_ k -> text "# " <> k]
                  , [ AssocR funKind $ \arg ret -> arg <> text " -> " <> ret ] ]
