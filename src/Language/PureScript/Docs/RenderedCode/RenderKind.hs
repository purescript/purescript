-- | Functions for producing RenderedCode values from PureScript Kind values.
--
module Language.PureScript.Docs.RenderedCode.RenderKind
  ( renderKind
  ) where

-- TODO: This is pretty much copied from Language.PureScript.Pretty.Kinds.
-- Ideally we would unify the two.

import Prelude.Compat

import Control.Arrow (ArrowPlus(..))
import Control.PatternArrows as PA

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Kinds

import Language.PureScript.Docs.RenderedCode.Types

typeLiterals :: Pattern () Kind RenderedCode
typeLiterals = mkPattern match
  where
  match (KUnknown u) =
    Just $ typeVar $ T.cons 'k' (T.pack (show u))
  match (NamedKind n) =
    Just $ kind n
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

-- | Generate RenderedCode value representing a Kind
renderKind :: Kind -> RenderedCode
renderKind
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchKind ()
  where
  matchKind :: Pattern () Kind RenderedCode
  matchKind = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchKind)

  operators :: OperatorTable () Kind RenderedCode
  operators =
    OperatorTable [ [ Wrap matchRow $ \_ k -> syntax "#" <> sp <> k]
                  , [ AssocR funKind $ \arg ret -> arg <> sp <> syntax "->" <> sp <> ret ] ]
