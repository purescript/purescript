{-# LANGUAGE GADTs #-}

-- | Subsumption checking
module Language.PureScript.TypeChecker.Subsumption
  ( subsumes
  ) where

import Prelude

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets)

import Language.PureScript.AST (ErrorMessageHint(..), Expr(..), pattern NullSourceAnn)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Errors (MultipleErrors, internalCompilerError)
import Language.PureScript.TypeChecker.Monad (CheckState(..), getHints, getTypeClassDictionaries, withErrorMessageHint)
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, skolemize)
import Language.PureScript.TypeChecker.Unify (freshTypeWithKind, substituteType, unifyishRowTypes, unifyTypes)
import Language.PureScript.Types (SourceType, Type(..), eqType, replaceTypeVars)

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

-- | Check that one type (ty2) subsumes another (ty1), rethrowing errors to provide a better error message
subsumes
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> SourceType
  -> m (Expr -> Expr)
subsumes ty1 ty2 =
  withErrorMessageHint (ErrorInSubsumption ty1 ty2) $
    subsumes' SElaborate ty1 ty2

-- | Check that one type subsumes another
subsumes'
  :: forall m mode
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => ModeSing mode
  -> SourceType
  -> SourceType
  -> m (Coercion mode)
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
subsumes' mode (TypeApp s1 f1@(TypeConstructor _ C.Record) r1) (TypeApp s2 f2@(TypeConstructor _ C.Record) r2) = do
  subst <- gets checkSubstitution
  unifyishRowTypes (TypeApp s1 f1) (TypeApp s2 f2) (subsumes' SNoElaborate) (substituteType subst r1) (substituteType subst r2)
  -- Nothing was elaborated, return the default coercion
  return (defaultCoercion mode)
subsumes' mode ty1 ty2@(TypeApp _ obj _) | obj == tyRecord =
  subsumes' mode ty2 ty1
subsumes' mode ty1 ty2 = do
  unifyTypes ty1 ty2
  -- Nothing was elaborated, return the default coercion
  return (defaultCoercion mode)
