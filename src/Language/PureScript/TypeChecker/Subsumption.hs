{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Subsumption checking
module Language.PureScript.TypeChecker.Subsumption
  ( subsumes
  ) where

import Prelude.Compat

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..))

import Data.Foldable (for_)
import Data.List (uncons)
import Data.List.Ordered (minusBy')
import Data.Ord (comparing)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.Types

-- | Subsumption can operate in two modes:
--
-- * Elaboration mode, in which we try to insert type class dictionaries
-- * No-elaboration mode, in which we do not insert dictionaries
--
-- Some subsumption rules apply in both modes, and others are specific to
-- certain modes.
--
-- The subsumption algorithm follows the structure of the types in question,
-- and we can switch into no-elaboration mode when we move under a type
-- constructor where we can no longer insert dictionaries, e.g. into the fields
-- of a record.
data Mode = Elaborate | NoElaborate

-- | Value-level proxies for the two modes
data ModeSing (mode :: Mode) where
  SElaborate   :: ModeSing 'Elaborate
  SNoElaborate :: ModeSing 'NoElaborate

-- | This type family tracks what evidence we return from 'subsumes' for each
-- mode.
type family Coercion (mode :: Mode) where
  -- When elaborating, we generate a coercion
  Coercion 'Elaborate = Expr -> Expr
  -- When we're not elaborating, we don't generate coercions
  Coercion 'NoElaborate = ()

-- | The default coercion for each mode.
defaultCoercion :: ModeSing mode -> Coercion mode
defaultCoercion SElaborate   = id
defaultCoercion SNoElaborate = ()

-- | Check that one type subsumes another, rethrowing errors to provide a better error message
subsumes
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Type
  -> Type
  -> m (Expr -> Expr)
subsumes ty1 ty2 =
  withErrorMessageHint (ErrorInSubsumption ty1 ty2) $
    subsumes' SElaborate ty1 ty2

-- | Check that one type subsumes another
subsumes'
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => ModeSing mode
  -> Type
  -> Type
  -> m (Coercion mode)
subsumes' mode (ForAll ident ty1 _) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  subsumes' mode replaced ty2
subsumes' mode ty1 (ForAll ident ty2 sco) =
  case sco of
    Just sco' -> do
      sko <- newSkolemConstant
      let sk = skolemize ident sko sco' Nothing ty2
      subsumes' mode ty1 sk
    Nothing -> internalError "subsumes: unspecified skolem scope"
subsumes' mode (TypeApp (TypeApp f1 arg1) ret1) (TypeApp (TypeApp f2 arg2) ret2) | f1 == tyFunction && f2 == tyFunction = do
  subsumes' SNoElaborate arg2 arg1
  subsumes' SNoElaborate ret1 ret2
  -- Nothing was elaborated, return the default coercion
  return (defaultCoercion mode)
subsumes' mode (KindedType ty1 _) ty2 =
  subsumes' mode ty1 ty2
subsumes' mode ty1 (KindedType ty2 _) =
  subsumes' mode ty1 ty2
-- Only check subsumption for constrained types when elaborating.
-- Otherwise fall back to unification.
subsumes' SElaborate (ConstrainedType con ty1) ty2 = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  elaborate <- subsumes' SElaborate ty1 ty2
  let addDicts val = App val (TypeClassDictionary con dicts hints)
  return (elaborate . addDicts)
subsumes' mode (TypeApp f1 r1) (TypeApp f2 r2) | f1 == tyRecord && f2 == tyRecord = do
    let (common, ((ts1', r1'), (ts2', r2'))) = alignRowsWith (subsumes' SNoElaborate) r1 r2
    -- For { ts1 | r1 } to subsume { ts2 | r2 } when r1 is empty (= we're working with a closed row),
    -- every property in ts2 must appear in ts1. If not, then the candidate expression is missing a required property.
    -- Conversely, when r2 is empty, every property in ts1 must appear in ts2, or else the expression has
    -- an additional property which is not allowed.
    when (r1' == REmpty)
      (for_ (firstMissingProp ts2' ts1') (throwError . errorMessage . PropertyIsMissing . fst))
    when (r2' == REmpty)
      (for_ (firstMissingProp ts1' ts2') (throwError . errorMessage . AdditionalProperty . fst))
    -- Check subsumption for common labels
    sequence_ common
    unifyTypes (rowFromList (ts1', r1')) (rowFromList (ts2', r2'))
    -- Nothing was elaborated, return the default coercion
    return (defaultCoercion mode)
  where
    -- Find the first property that's in the first list (of tuples) but not in the second
    firstMissingProp t1 t2 = fst <$> uncons (minusBy' (comparing fst) t1 t2)
subsumes' mode ty1 ty2@(TypeApp obj _) | obj == tyRecord =
  subsumes' mode ty2 ty1
subsumes' mode ty1 ty2 = do
  unifyTypes ty1 ty2
  -- Nothing was elaborated, return the default coercion
  return (defaultCoercion mode)
