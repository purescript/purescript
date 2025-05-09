{-# LANGUAGE GADTs #-}

-- | Subsumption checking
module Language.PureScript.TypeChecker.Subsumption
  ( subsumes
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError(..))

import Data.Foldable (for_)
import Data.List (uncons)
import Data.List.Ordered (minusBy')
import Data.Ord (comparing)

import Language.PureScript.AST (ErrorMessageHint(..), Expr(..), pattern NullSourceAnn)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Errors (SimpleErrorMessage(..), errorMessage, internalCompilerError)
import Language.PureScript.TypeChecker.Monad (getHints, getTypeClassDictionaries, withErrorMessageHint, TypeCheckM)
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, skolemize)
import Language.PureScript.TypeChecker.Unify (alignRowsWith, freshTypeWithKind, unifyTypes)
import Language.PureScript.Types (RowListItem(..), SourceType, Type(..), eqType, isREmpty, replaceTypeVars, rowFromList)

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
  :: ()
  => SourceType
  -> SourceType
  -> TypeCheckM (Expr -> Expr)
subsumes ty1 ty2 =
  withErrorMessageHint (ErrorInSubsumption ty1 ty2) $
    subsumes' SElaborate ty1 ty2

-- | Check that one type subsumes another
subsumes'
  :: ()
  => ModeSing mode
  -> SourceType
  -> SourceType
  -> TypeCheckM (Coercion mode)
subsumes' mode (ForAll _ _ ident mbK ty1 _) ty2 = do
  u <- maybe (internalCompilerError "Unelaborated forall") freshTypeWithKind mbK
  let replaced = replaceTypeVars ident u ty1
  subsumes' mode replaced ty2
subsumes' mode ty1 (ForAll _ _ ident mbK ty2 sco) =
  case sco of
    Just sco' -> do
      sko <- newSkolemConstant
      let sk = skolemize NullSourceAnn ident mbK sko sco' ty2
      subsumes' mode ty1 sk
    Nothing -> internalError "subsumes: unspecified skolem scope"
subsumes' mode (TypeApp _ (TypeApp _ f1 arg1) ret1) (TypeApp _ (TypeApp _ f2 arg2) ret2) | eqType f1 tyFunction && eqType f2 tyFunction = do
  subsumes' SNoElaborate arg2 arg1
  subsumes' SNoElaborate ret1 ret2
  -- Nothing was elaborated, return the default coercion
  return (defaultCoercion mode)
subsumes' mode (KindedType _ ty1 _) ty2 =
  subsumes' mode ty1 ty2
subsumes' mode ty1 (KindedType _ ty2 _) =
  subsumes' mode ty1 ty2
-- Only check subsumption for constrained types when elaborating.
-- Otherwise fall back to unification.
subsumes' SElaborate (ConstrainedType _ con ty1) ty2 = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  elaborate <- subsumes' SElaborate ty1 ty2
  let addDicts val = App val (TypeClassDictionary con dicts hints)
  return (elaborate . addDicts)
subsumes' mode (TypeApp _ f1 r1) (TypeApp _ f2 r2) | eqType f1 tyRecord && eqType f2 tyRecord = do
    let goWithLabel l t1 t2 = withErrorMessageHint (ErrorInRowLabel l) $ subsumes' SNoElaborate t1 t2
    let (common, ((ts1', r1'), (ts2', r2'))) = alignRowsWith goWithLabel r1 r2
    -- For { ts1 | r1 } to subsume { ts2 | r2 } when r1 is empty (= we're working with a closed row),
    -- every property in ts2 must appear in ts1. If not, then the candidate expression is missing a required property.
    -- Conversely, when r2 is empty, every property in ts1 must appear in ts2, or else the expression has
    -- an additional property which is not allowed.
    when (isREmpty r1')
      (for_ (firstMissingProp ts2' ts1') (throwError . errorMessage . PropertyIsMissing . rowListLabel))
    when (isREmpty r2')
      (for_ (firstMissingProp ts1' ts2') (throwError . errorMessage . AdditionalProperty . rowListLabel))
    -- Check subsumption for common labels
    sequence_ common
    -- Inject the info here
    unifyTypes (rowFromList (ts1', r1')) (rowFromList (ts2', r2'))
    -- Nothing was elaborated, return the default coercion
    return (defaultCoercion mode)
  where
    -- Find the first property that's in the first list (of tuples) but not in the second
    firstMissingProp t1 t2 = fst <$> uncons (minusBy' (comparing rowListLabel) t1 t2)
subsumes' mode ty1 ty2@(TypeApp _ obj _) | obj == tyRecord =
  subsumes' mode ty2 ty1
subsumes' mode ty1 ty2 = do
  unifyTypes ty1 ty2
  -- Nothing was elaborated, return the default coercion
  return (defaultCoercion mode)
