-- |
-- Pretty printer for kinds
--
module Language.PureScript.Pretty.Kinds
  ( prettyPrintKind
  ) where

import Prelude.Compat

import Control.Arrow (ArrowPlus(..))
import Control.PatternArrows as PA

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

import Language.PureScript.Crash
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Common

typeLiterals :: Pattern () Kind Text
typeLiterals = mkPattern match
  where
  match (KUnknown u) =
    Just $ T.cons 'k' (T.pack (show u))
  match (NamedKind name) =
    Just $ if isQualifiedWith (moduleNameFromString "Prim") name
      then runProperName (disqualify name)
      else showQualified runProperName name
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
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchKind ()
  where
  matchKind :: Pattern () Kind Text
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parensT matchKind)

  operators :: OperatorTable () Kind Text
  operators =
    OperatorTable [ [ Wrap matchRow $ \_ k -> "# " <> k]
                  , [ AssocR funKind $ \arg ret -> arg <> " -> " <> ret ] ]
