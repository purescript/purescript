-- |
-- Pretty printer for kinds
--
module Language.PureScript.Pretty.Kinds
  ( prettyPrintKind
  ) where

import Prelude.Compat

import Control.Arrow (ArrowPlus(..))
import Control.PatternArrows as PA

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

import Language.PureScript.Crash
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Common

typeLiterals :: Pattern () Kind String
typeLiterals = mkPattern match
  where
  match (KUnknown u) = Just $ 'u' : show u
  match (NamedKind name) = Just $ T.unpack (showQualified runProperName name)
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
prettyPrintKind :: Kind -> Text
prettyPrintKind
  -- TODO(Christoph): get rid of T.pack
  = T.pack
  . fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchKind ()
  where
  matchKind :: Pattern () Kind String
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchKind)

  operators :: OperatorTable () Kind String
  operators =
    OperatorTable [ [ Wrap matchRow $ \_ k -> "# " ++ k]
                  , [ AssocR funKind $ \arg ret -> arg ++ " -> " ++ ret ] ]
