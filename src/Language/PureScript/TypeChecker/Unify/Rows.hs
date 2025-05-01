module Language.PureScript.TypeChecker.Unify.Rows
  ( unifyishRows
  , isTypesDoNotUnify
  , isKindsDoNotUnify
  ) where

import Prelude

import Control.Monad (forM, when)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.State.Class (MonadState (..))
import Data.Either (lefts)
import Data.Foldable (fold)

import Language.PureScript.Errors (ErrorMessage (..), ErrorMessageHint (..), MultipleErrors (..), SimpleErrorMessage (..), SourceAnn, nonEmpty)
import Language.PureScript.Label (Label)
import Language.PureScript.TypeChecker.Monad (CheckState (..), withErrorMessageHint)
import Language.PureScript.Types (RowListItem (..), SourceType, alignRowsWith)

-- | Do a unify-like operation on two rows, updating the current substitution
--
-- Common labels are identified and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate.
unifyishRows ::
  forall m.
  (MonadError MultipleErrors m, MonadState CheckState m) =>
  ((([RowListItem SourceAnn], SourceType), ([RowListItem SourceAnn], SourceType)) -> m Bool) ->
  (SimpleErrorMessage -> Bool) ->
  (SourceType -> SourceType -> m MultipleErrors) ->
  (SourceType -> SourceType -> m ()) ->
  SourceType ->
  SourceType ->
  m ()
unifyishRows onUnifyTails isExpectedError buildError onRecurse r1 r2 = do
  -- First, `onRecurse` the types in aligned labels, but don't throw any errors yet.
  alignedErr <- fold . lefts <$> forM matches tryError
  -- Now `onTails` the tails; this must happen after matching the types in aligned
  -- labels, so that any unknowns solved from the aligned labels don't appear
  -- in the error message produced if the tails don't unify.
  tailErr <-
    tryError (onUnifyTails rest) >>= \case
      -- If an error was raised during unification, return that.
      Left err -> pure err
      -- If tails did unify, return an empty error (will be filtered below).
      Right True -> pure mempty
      -- If tails didn't unify, then suppress any TypesDoNotUnify errors produced
      -- during unification of aligned labels. The TypesDoNotUnify created here
      -- will include any such mismatches.
      Right False -> do
        initialErr <- buildError r1 r2
        -- Throw immediately; we're including the aligned-label errors we want to
        -- include.
        throwError $
          MultipleErrors (filter isNotExpectedError $ runMultipleErrors alignedErr)
            <> initialErr
  let combinedErr = alignedErr <> tailErr
  when (nonEmpty combinedErr) $ throwError combinedErr
  where
    -- exported from Control.Monad.Error.Class in mtl >= 2.3
    tryError :: forall a. m a -> m (Either MultipleErrors a)
    tryError u = catchError (Right <$> u) (pure . Left)

    onRecurseWithLabel :: Label -> SourceType -> SourceType -> m ()
    onRecurseWithLabel l t1 t2 = withErrorMessageHint (ErrorInRowLabel l) $ onRecurse t1 t2

    (matches, rest) = alignRowsWith onRecurseWithLabel r1 r2

    isNotExpectedError :: ErrorMessage -> Bool
    isNotExpectedError = \case
      ErrorMessage _ x | isExpectedError x -> False
      _ -> True

isTypesDoNotUnify :: SimpleErrorMessage -> Bool
isTypesDoNotUnify = \case
  TypesDoNotUnify{} -> True
  _ -> False

isKindsDoNotUnify :: SimpleErrorMessage -> Bool
isKindsDoNotUnify = \case
  KindsDoNotUnify{} -> True
  _ -> False
