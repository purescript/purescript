-- |
-- Subsumption checking
--
module Language.PureScript.TypeChecker.Subsumption
  ( subsumes
  ) where

import Prelude.Compat

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets)

import Data.Foldable (for_)
import Data.List (sortBy, uncons)
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

-- | Check that one type subsumes another, rethrowing errors to provide a better error message
subsumes :: (MonadError MultipleErrors m, MonadState CheckState m) => Maybe Expr -> Type -> Type -> m (Maybe Expr)
subsumes val ty1 ty2 = withErrorMessageHint (ErrorInSubsumption ty1 ty2) $ subsumes' val ty1 ty2

-- | Check that one type subsumes another
subsumes' :: (MonadError MultipleErrors m, MonadState CheckState m) =>
  Maybe Expr ->
  Type ->
  Type ->
  m (Maybe Expr)
subsumes' val (ForAll ident ty1 _) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  subsumes val replaced ty2
subsumes' val ty1 (ForAll ident ty2 sco) =
  case sco of
    Just sco' -> do
      sko <- newSkolemConstant
      let sk = skolemize ident sko sco' Nothing ty2
      subsumes val ty1 sk
    Nothing -> internalError "subsumes: unspecified skolem scope"
subsumes' val (TypeApp (TypeApp f1 arg1) ret1) (TypeApp (TypeApp f2 arg2) ret2) | f1 == tyFunction && f2 == tyFunction = do
  _ <- subsumes Nothing arg2 arg1
  _ <- subsumes Nothing ret1 ret2
  return val
subsumes' val (KindedType ty1 _) ty2 =
  subsumes val ty1 ty2
subsumes' val ty1 (KindedType ty2 _) =
  subsumes val ty1 ty2
subsumes' (Just val) (ConstrainedType constraints ty1) ty2 = do
  dicts <- getTypeClassDictionaries
  hints <- gets checkHints
  subsumes' (Just $ foldl App val (map (\cs -> TypeClassDictionary cs dicts hints) constraints)) ty1 ty2
subsumes' val (TypeApp f1 r1) (TypeApp f2 r2) | f1 == tyRecord && f2 == tyRecord = do
  let
    (ts1, r1') = rowToList r1
    (ts2, r2') = rowToList r2
    ts1' = sortBy (comparing fst) ts1
    ts2' = sortBy (comparing fst) ts2
  -- For { ts1 | r1 } to subsume { ts2 | r2 } when r1 is empty (= we're working with a closed row),
  -- every property in ts2 must appear in ts1. If not, then the candidate expression is missing a required property.
  -- Conversely, when r2 is empty, every property in ts1 must appear in ts2, or else the expression has
  -- an additional property which is not allowed.
  when (r1' == REmpty)
    (for_ (firstMissingProp ts2' ts1') (throwError . errorMessage . PropertyIsMissing . fst))
  when (r2' == REmpty)
    (for_ (firstMissingProp ts1' ts2') (throwError . errorMessage . AdditionalProperty . fst))
  go ts1' ts2' r1' r2'
  return val
  where
  go [] ts2 r1' r2' = unifyTypes r1' (rowFromList (ts2, r2'))
  go ts1 [] r1' r2' = unifyTypes r2' (rowFromList (ts1, r1'))
  go ((p1, ty1) : ts1) ((p2, ty2) : ts2) r1' r2'
    | p1 == p2 = do _ <- subsumes Nothing ty1 ty2
                    go ts1 ts2 r1' r2'
    | p1 < p2 = do rest <- freshType
                   -- What happens next is a bit of a hack.
                   -- TODO: in the new type checker, object properties will probably be restricted to being monotypes
                   -- in which case, this branch of the subsumes function should not even be necessary.
                   unifyTypes r2' (RCons p1 ty1 rest)
                   go ts1 ((p2, ty2) : ts2) r1' rest
    | otherwise = do rest <- freshType
                     unifyTypes r1' (RCons p2 ty2 rest)
                     go ((p1, ty1) : ts1) ts2 rest r2'
  -- Find the first property that's in the first list (of tuples) but not in the second
  firstMissingProp t1 t2 = fst <$> uncons (minusBy' (comparing fst) t1 t2)
subsumes' val ty1 ty2@(TypeApp obj _) | obj == tyRecord = subsumes val ty2 ty1
subsumes' val ty1 ty2 = do
  unifyTypes ty1 ty2
  return val
