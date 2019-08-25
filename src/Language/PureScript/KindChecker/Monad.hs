module Language.PureScript.KindChecker.Monad where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState, get, gets, modify, put, state)
import Control.Monad.State.Strict (State, runState)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Language.PureScript.Names
import Language.PureScript.Types

type MonadCheck m = (MonadState Context m, MonadError KindError m)

data KindError
  = KindDoesNotResultInType SourceType
  | SignatureUnknownVars [Text]
  | CycleInSignature [Text]
  | CycleInBinderList [Text]
  | UnknownNotInScope Int
  | VarNotInScope Text
  | TypeNotInScope (Qualified (ProperName 'TypeName))
  | InfiniteKind SourceType
  | DoesNotUnify SourceType SourceType
  | CannotApplyType (SourceType, SourceType) SourceType
  | CannotApplyKind (SourceType, SourceType) SourceType
  | ElaboratedKindIsNotType SourceType
  | QuantificationCheckFailure [Text]
  | InternalError String (Maybe SourceType)
  | InvalidKind SourceType
  | InvalidType SourceType
  deriving (Show, Eq)

data Level
  = LvlAt !Int
  | LvlBefore !Level !Int
  deriving (Show, Eq)

instance Ord Level where
  compare = curry $ \case
    (LvlAt a, LvlAt b) ->
      compare a b
    (LvlBefore a1 a2, LvlBefore b1 b2) ->
      compare a2 b2 <> compare a1 b1
    (LvlBefore _ a2, LvlAt b) ->
      compare a2 b <> LT
    (LvlAt a, LvlBefore _ b2) ->
      compare a b2 <> GT

shallowLt :: Int -> Level -> Bool
shallowLt i = \case
  LvlAt i' -> i' < i
  LvlBefore _ i' -> i' < i

shallowLevel :: Level -> Int
shallowLevel = \case
  LvlAt i -> i
  LvlBefore _ i -> i

before :: Int -> Level -> Level
before i = \case
  LvlAt i1 ->
    LvlBefore (LvlAt i) i1
  LvlBefore (LvlAt i1) i2 ->
    LvlBefore (LvlBefore (LvlAt i) i1) i2
  LvlBefore (LvlBefore (LvlAt i1) i2) i3 ->
    LvlBefore (LvlBefore (LvlBefore (LvlAt i) i1) i2) i3
  LvlBefore (LvlBefore (LvlBefore a i1) i2) i3 ->
    LvlBefore (LvlBefore (LvlBefore (go a) i1) i2) i3
  where
  go = \case
    LvlAt i' ->
      LvlBefore (LvlAt i) i'
    LvlBefore a i' ->
      LvlBefore (go a) i'

mkLevel :: Int -> Maybe Level -> Level
mkLevel i = maybe (LvlAt i) (before i)

data Solution = Solution
  { solKind :: !SourceType
  , solType :: !SourceType
  , solUnsolved :: IS.IntSet
  } deriving (Show, Eq)

data Context = Context
  { ctxFresh :: !Int
  , ctxLevel :: !Int
  , ctxScope :: NE.NonEmpty TypeScope
  , ctxSolutions :: !(IM.IntMap Solution)
  , ctxTypes :: !(M.Map (Qualified (ProperName 'TypeName)) ScopeValue)
  } deriving (Show, Eq)

data TypeScope = TypeScope
  { tsLevel :: !Int
  , tsUnsolved :: !(IM.IntMap ScopeValue)
  , tsVars :: !(M.Map Text ScopeValue)
  } deriving (Show, Eq)

data ScopeValue = ScopeValue
  { scLevel :: !Level
  , scType :: !SourceType
  , scUnsolved :: IS.IntSet
  } deriving (Show, Eq)

emptyContext :: Context
emptyContext = Context
  { ctxFresh = 0
  , ctxLevel = 0
  , ctxScope = pure (TypeScope 0 mempty mempty)
  , ctxSolutions = mempty
  , ctxTypes = mempty
  }

modifyScope :: (TypeScope -> TypeScope) -> Level -> NE.NonEmpty TypeScope -> NE.NonEmpty TypeScope
modifyScope k lvl = NE.fromList . go . NE.toList
  where
  bound =
    shallowLevel lvl

  go = \case
    [] -> []
    (ts : tss)
      | tsLevel ts <= bound -> k ts : tss
      | otherwise -> ts : go tss

insertUnsolved :: Int -> ScopeValue -> TypeScope -> TypeScope
insertUnsolved i value ts = ts { tsUnsolved = IM.insert i value $ tsUnsolved ts }

insertVar :: Text -> ScopeValue -> TypeScope -> TypeScope
insertVar var value ts = ts { tsVars = M.insert var value $ tsVars ts }

lookupType :: MonadState Context m => Qualified (ProperName 'TypeName) -> m (Maybe ScopeValue)
lookupType n =
  gets (M.lookup n . ctxTypes) >>= \case
    Nothing -> pure Nothing
    Just sc -> do
      solved <- gets ctxSolutions
      let (sc', solved') = applyScopeValue solved sc
      when (IS.size (scUnsolved sc') < IS.size (scUnsolved sc)) $ do
        modify $ \ctx -> ctx
          { ctxSolutions = solved'
          , ctxTypes = M.insert n sc' (ctxTypes ctx)
          }
      pure $ Just sc'

lookupUnsolved :: MonadState Context m => Int -> m (Maybe ScopeValue)
lookupUnsolved u = gets $ foldr ((<|>) . IM.lookup u . tsUnsolved) Nothing . ctxScope

lookupVar :: MonadState Context m => Text -> m (Maybe ScopeValue)
lookupVar v = gets (foldr ((<|>) . M.lookup v . tsVars) Nothing . ctxScope)

unknown :: MonadState Context m => m Int
unknown = state $ \ctx ->
  ( ctxFresh ctx
  , ctx { ctxFresh = ctxFresh ctx + 1 }
  )

extendType :: MonadState Context m => Qualified (ProperName 'TypeName) -> SourceType -> m ()
extendType n ty = modify $ \ctx -> do
  let
    next = ctxFresh ctx
    value = ScopeValue (LvlAt next) ty (unknowns ty)
  ctx
    { ctxLevel = next + 1
    , ctxTypes = M.insert n value $ ctxTypes ctx
    }

extendUnsolved :: MonadState Context m => Maybe Level -> Int -> SourceType -> m ()
extendUnsolved lvl i ty = modify $ \ctx -> do
  let
    next = ctxLevel ctx
    lvl' = mkLevel next lvl
    value = ScopeValue lvl' ty (unknowns ty)
  ctx
    { ctxLevel = next + 1
    , ctxScope = modifyScope (insertUnsolved i value) lvl' $ ctxScope ctx
    }

extendVar :: MonadState Context m => Text -> SourceType -> m ()
extendVar var ty = modify $ \ctx -> do
  let
    next = ctxLevel ctx
    lvl = LvlAt next
    value = ScopeValue lvl ty (unknowns ty)
  ctx
    { ctxLevel = next + 1
    , ctxScope = modifyScope (insertVar var value) lvl $ ctxScope ctx
    }

solve :: MonadState Context m => Int -> SourceType -> SourceType -> m ()
solve i knd ty = modify $ \ctx ->
  ctx { ctxSolutions = IM.insert i (Solution knd ty (unknowns ty)) $ ctxSolutions ctx }

newScope :: MonadState Context m => m Int
newScope = state $ \ctx -> do
  let
    scope = TypeScope (ctxLevel ctx) mempty mempty
    ctx' = ctx { ctxScope = scope `NE.cons` ctxScope ctx }
  (ctxLevel ctx, ctx')

dropScope :: MonadState Context m => m (IM.IntMap ScopeValue)
dropScope = state $ \ctx -> do
  let
    TypeScope _ uns _ NE.:| scope' = ctxScope ctx
    uns' = IM.filterWithKey (\i _ -> IM.notMember i (ctxSolutions ctx)) uns
    (uns'', solved) = applyUnknowns (ctxSolutions ctx) uns'
    ctx' = ctx { ctxScope = NE.fromList scope', ctxSolutions = solved }
  (uns'', ctx')

scopedWithUnsolved :: MonadState Context m => m a -> m (a, IM.IntMap ScopeValue)
scopedWithUnsolved act = do
  _ <- newScope
  res <- act
  (res,) <$> dropScope

scoped :: MonadState Context m => m a -> m a
scoped = fmap fst . scopedWithUnsolved

apply :: MonadState Context m => SourceType -> m SourceType
apply ty = state $ \ctx -> do
  let
    (res, solved) = applySolutionsToType (ctxSolutions ctx) ty
    ctx' = ctx { ctxSolutions = solved }
  (res, ctx')

data SolutionState = SolutionState
  { ssUnsolved :: !IS.IntSet
  , ssSolutions :: !(IM.IntMap Solution)
  }

applyScopeValue :: IM.IntMap Solution -> ScopeValue -> (ScopeValue, IM.IntMap Solution)
applyScopeValue initialSolved sv@(ScopeValue lvl ty uns)
  | any (flip IM.member initialSolved) $ IS.toList uns = do
      let
        (ty', (SolutionState uns' solved)) =
          flip runState (SolutionState mempty initialSolved)
            $ applySolutionsToType' ty
      (ScopeValue lvl ty' uns', solved)
  | otherwise =
      (sv, initialSolved)

applyUnknowns :: IM.IntMap Solution -> IM.IntMap ScopeValue -> (IM.IntMap ScopeValue, IM.IntMap Solution)
applyUnknowns initialSolved =
  fmap ssSolutions . flip runState (SolutionState mempty initialSolved) . applyUnknowns'

applyUnknowns' :: IM.IntMap ScopeValue -> State SolutionState (IM.IntMap ScopeValue)
applyUnknowns' = traverse go
  where
  go (ScopeValue lvl ty _) = do
    modify $ \(SolutionState _ solved) ->
      SolutionState mempty solved
    ty' <- applySolutionsToType' ty
    uns <- gets ssUnsolved
    pure $ ScopeValue lvl ty' uns

applySolutionsToType :: IM.IntMap Solution -> SourceType -> (SourceType, IM.IntMap Solution)
applySolutionsToType initialSolved =
  fmap ssSolutions . flip runState (SolutionState mempty initialSolved) . applySolutionsToType'

applySolutionsToType' :: SourceType -> State SolutionState SourceType
applySolutionsToType' = go
  where
  go = everywhereOnTypesM $ \ty -> case ty of
    TUnknown _ i -> do
      SolutionState unks solved <- get
      case IM.lookup i solved of
        Just (Solution knd ty' unks')
          | any (flip IM.member solved) $ IS.toList unks' -> do
              put $ SolutionState mempty solved
              ty'' <- go ty'
              state $ \(SolutionState unks'' solved') ->
                (ty'', SolutionState unks (IM.insert i (Solution knd ty'' unks'') solved'))
          | otherwise ->
              pure ty'
        _ -> do
          put $ SolutionState (IS.insert i unks) solved
          pure ty
    _ ->
      pure ty
