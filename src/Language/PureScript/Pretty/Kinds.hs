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

typeLiterals :: Pattern () (Kind a) Text
typeLiterals = mkPattern match
  where
  match (KUnknown _ u) =
    Just $ T.cons 'k' (T.pack (show u))
  match (NamedKind _ name) =
    Just $ if isQualifiedWith (moduleNameFromString "Prim") name
      then runProperName (disqualify name)
      else showQualified runProperName name
  match _ = Nothing

matchRow :: Pattern () (Kind a) ((), Kind a)
matchRow = mkPattern match
  where
  match (Row _ k) = Just ((), k)
  match _ = Nothing

funKind :: Pattern () (Kind a) (Kind a, Kind a)
funKind = mkPattern match
  where
  match (FunKind _ arg ret) = Just (arg, ret)
  match _ = Nothing

-- | Generate a pretty-printed string representing a Kind
prettyPrintKind :: Kind a -> Text
prettyPrintKind
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchKind ()
  where
  matchKind :: Pattern () (Kind a) Text
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parensT matchKind)

  operators :: OperatorTable () (Kind a) Text
  operators =
    OperatorTable [ [ Wrap matchRow $ \_ k -> "# " <> k]
                  , [ AssocR funKind $ \arg ret -> arg <> " -> " <> ret ] ]
