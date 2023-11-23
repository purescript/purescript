{-# LANGUAGE TemplateHaskell #-}
-- | This module performs limited common subexpression elimination
module Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions) where

import Protolude hiding (pass)

import Control.Lens (At(..), makeLenses, non, view, (%~), (.=), (.~), (<>~), (^.))
import Control.Monad.Supply (Supply)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.RWS (MonadWriter, RWST, censor, evalRWST, listen, pass, tell)
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose (Compose(..))
import Data.IntMap.Monoidal qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Semigroup (Min(..))
import Data.Semigroup.Generic (GenericSemigroupMonoid(..))

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import Language.PureScript.CoreFn.Meta (Meta(IsSyntheticApp))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues, traverseCoreFn)
import Language.PureScript.Environment (dictTypeName)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), Qualified(..), QualifiedBy(..), freshIdent, runIdent, toMaybeModuleName)
import Language.PureScript.PSString (decodeString)

-- |
-- `discuss f m` is an action that listens to the output of `m`, passes that
-- and its value through `f`, and uses (only) the value of the result to set
-- the new value and output. (Any output produced via the monad in `f` is
-- ignored, though other monadic effects will hold.)
--
discuss :: MonadWriter w m => ((a, w) -> m (b, w)) -> m a -> m b
discuss f = pass . fmap (second const) . (f <=< listen)

-- |
-- Modify the target of an optic in the state with a monadic computation that
-- returns some extra information of type `r` in a tuple.
--
-- I would prefer that this be a named function, but I don't know what to name
-- it. I went with symbols instead because the function that this operator most
-- resembles is `(%%=)`, which doesn't have a textual name as far as I know.
-- Compare the following (approximate) types:
--
-- @
-- (%%=)  :: MonadState s m => Lens s s a b -> (a ->   (r, b)) -> m r
-- (%%<~) :: MonadState s m => Lens s s a b -> (a -> m (r, b)) -> m r
-- @
--
-- Replacing the `=` with `<~` was inspired by analogy with the following pair:
--
-- @
-- (.=) :: MonadState s m => Lens s s a b ->   b -> m ()
-- (<~) :: MonadState s m => Lens s s a b -> m b -> m ()
-- @
--
-- I regret any confusion that ensues.
--
-- Note that there are two interpretations that could reasonably be expected
-- for this type.
--
-- @
-- (%%<~) :: MonadState s m => Lens s s a b -> (a -> m (r, b)) -> m r
-- @
--
-- One is:
-- * Get the focused `a` value from the monad
-- * Run the computation
-- * Get the new state from the returned monad
-- * Take the returned `b` value and set it in the new state
--
-- The other is:
-- * Get the focused `a` value from the monad
-- * Run the computation
-- * Take the returned `b` value and set it in the *original* state
-- * Put the result into the returned monad
--
-- This operator corresponds to the second interpretation. The purpose of this,
-- and part of the purpose of having this operator at all instead of composing
-- simpler operators, is to enable using the lens only once (on the original
-- state) instead of twice (for a get and a set on different states).
--
(%%<~)
  :: MonadState s m
  => ((a -> Compose m ((,) r) b) -> s -> Compose m ((,) r) s)
     -- ^ please read as Lens s s a b
  -> (a -> m (r, b))
  -> m r
l %%<~ f = get >>= getCompose . l (Compose . f) >>= state . const
infix 4 %%<~

-- |
-- A PluralityMap is like a weaker multiset: like a multiset, it can hold
-- several of the same value, but instead of keeping track of their exact
-- counts, it only records whether there is one (False) or more than one
-- (True).
--
newtype PluralityMap k = PluralityMap { getPluralityMap :: M.Map k Bool }

instance Ord k => Semigroup (PluralityMap k) where
  PluralityMap l <> PluralityMap r =
    let
      l' = M.mapWithKey (\k -> (|| k `M.member` r)) l
    in PluralityMap $ l' `M.union` r

instance Ord k => Monoid (PluralityMap k) where
  mempty = PluralityMap M.empty

data BindingType = NonRecursive | Recursive deriving Eq

-- |
-- Record summary data about an expression.
--
data CSESummary = CSESummary
  { _scopesUsed    :: IS.IntSet
    -- ^ set of the scope numbers used in this expression
  , _noFloatWithin :: Maybe (Min Int)
    -- ^ optionally a scope within which this expression is not to be floated
    -- (because the expression uses an identifier bound recursively in that
    -- scope)
  , _plurality     :: PluralityMap Ident
    -- ^ which floated identifiers are used more than once in this expression
    -- (note that a single use inside an Abs will be considered multiple uses,
    -- as this pass doesn't know when/how many times an Abs will be executed)
  , _newBindings   :: IM.MonoidalIntMap [(Ident, (PluralityMap Ident, Expr Ann))]
    -- ^ floated bindings, organized by scope number
  , _toBeReinlined :: M.Map Ident (Expr Ann)
    -- ^ a map of floated identifiers that did not end up getting bound and
    -- will need to be reinlined at the end of the pass
  }
  deriving Generic
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid CSESummary

-- |
-- Append a value at a given scope depth.
--
addToScope :: Semigroup v => Int -> v -> IM.MonoidalIntMap v -> IM.MonoidalIntMap v
addToScope depth v
  = IM.alter (Just . maybe v (<> v)) depth

-- |
-- Remove and return an entire scope from a map of bindings.
--
popScope :: Monoid v => Int -> IM.MonoidalIntMap v -> (v, IM.MonoidalIntMap v)
popScope depth
  = first fold . IM.updateLookupWithKey (\_ _ -> Nothing) depth

-- |
-- Describe the context of an expression.
--
data CSEEnvironment = CSEEnvironment
  { _depth :: Int
    -- ^ number of enclosing binding scopes (this includes not only Abs, but
    -- Let and CaseAlternative bindings)
  , _deepestTopLevelScope :: Int
    -- ^ number of enclosing binding scopes outside the first Abs; used to
    -- decide whether to qualify floated identifiers
  , _bound :: M.Map Ident (Int, BindingType)
    -- ^ map from identifiers to depth in which they are bound and whether
    -- or not the binding is recursive
  }

makeLenses ''CSESummary
makeLenses ''CSEEnvironment

-- |
-- Map from the shape of an expression to an identifier created to represent
-- that expression, organized by scope depth.
--
type CSEState = IM.MonoidalIntMap (M.Map (Expr ()) Ident)

-- |
-- The monad in which CSE takes place.
--
type CSEMonad a = RWST CSEEnvironment CSESummary CSEState Supply a

type HasCSEReader = MonadReader CSEEnvironment
type HasCSEWriter = MonadWriter CSESummary
type HasCSEState = MonadState CSEState

-- |
-- Run a CSEMonad computation; the return value is augmented with a map of
-- identifiers that should be replaced in the final expression because they
-- didn't end up needing to be floated.
--
runCSEMonad :: CSEMonad a -> Supply (a, M.Map Ident (Expr Ann))
runCSEMonad x = second (^. toBeReinlined) <$> evalRWST x (CSEEnvironment 0 0 M.empty) IM.empty

-- |
-- Mark all expressions floated out of this computation as "plural". This pass
-- assumes that any given Abs may be invoked multiple times, so any expressions
-- inside the Abs but floated out of it also count as having multiple uses,
-- even if they only appear once within the Abs. Consequently, any expressions
-- that can be floated out of an Abs won't be reinlined at the end.
--
enterAbs :: HasCSEWriter m => m a -> m a
enterAbs = censor $ plurality %~ PluralityMap . fmap (const True) . getPluralityMap

-- |
-- Run the provided computation in a new scope.
--
newScope :: (HasCSEReader m, HasCSEWriter m) => Bool -> (Int -> m a) -> m a
newScope isTopLevel body = local goDeeper $ do
  d <- view depth
  censor (filterToDepth d) (body d)
  where
  filterToDepth d
    = (scopesUsed %~ IS.filter (< d))
    . (noFloatWithin %~ find (< Min d))
  goDeeper env@CSEEnvironment{..} =
    if isTopLevel
    then env{ _depth = depth', _deepestTopLevelScope = depth' }
    else env{ _depth = depth' }
    where
    depth' = succ _depth

-- |
-- Record a list of identifiers as being bound in the given scope.
--
withBoundIdents :: HasCSEReader m => [Ident] -> (Int, BindingType) -> m a -> m a
withBoundIdents idents t = local (bound %~ flip (foldl' (flip (flip M.insert t))) idents)

-- |
-- Run the provided computation in a new scope in which the provided
-- identifiers are bound non-recursively.
--
newScopeWithIdents :: (HasCSEReader m, HasCSEWriter m) => Bool -> [Ident] -> m a -> m a
newScopeWithIdents isTopLevel idents = newScope isTopLevel . flip (withBoundIdents idents . (, NonRecursive))

-- |
-- Produce, or retrieve from the state, an identifier for referencing the given
-- expression, at and below the given depth.
--
generateIdentFor :: (HasCSEState m, MonadSupply m) => Int -> Expr () -> m (Bool, Ident)
generateIdentFor d e = at d . non mempty . at e %%<~ \case
  Nothing    -> freshIdent (nameHint e) <&> \ident -> ((True, ident), Just ident)
  Just ident -> pure ((False, ident), Just ident)
  -- A reminder: as with %%=, the first element of the returned pair is the
  -- final result of the expression, and the second element is the value to
  -- stuff back through the lens into the state. (The difference is that %%<~
  -- enables doing monadic work in the RHS, namely `freshIdent` here.)
  where
  nameHint = \case
    App _ v1 v2
      | Var _ n <- v1
      , fmap (ProperName . runIdent) n == fmap dictTypeName C.IsSymbol
      , Literal _ (ObjectLiteral [(_, Abs _ _ (Literal _ (StringLiteral str)))]) <- v2
      , Just decodedStr <- decodeString str
        -> decodedStr <> "IsSymbol"
      | otherwise
        -> nameHint v1
    Var _ (Qualified _ ident)
      | Ident name             <- ident -> name
      | GenIdent (Just name) _ <- ident -> name
    Accessor _ prop _
      | Just decodedProp <- decodeString prop -> decodedProp
    _ -> "ref"

nullAnn :: Ann
nullAnn = (nullSourceSpan, [], Nothing)

-- |
-- Use a map to substitute local Vars in a list of Binds.
--
replaceLocals :: M.Map Ident (Expr Ann) -> [Bind Ann] -> [Bind Ann]
replaceLocals m = if M.null m then identity else map f' where
  (f', g', _) = everywhereOnValues identity f identity
  f e@(Var _ (Qualified _ ident)) = maybe e g' $ ident `M.lookup` m
  f e = e

-- |
-- Store in the monad a new binding for the given expression, returning a Var
-- referencing it. The provided CSESummary will be transformed to reflect the
-- replacement.
--
floatExpr
  :: (HasCSEReader m, HasCSEState m, MonadSupply m)
  => QualifiedBy
  -> (Expr Ann, CSESummary)
  -> m (Expr Ann, CSESummary)
floatExpr topLevelQB = \case
  (e, w@CSESummary{ _noFloatWithin = Nothing, .. }) -> do
    let deepestScope = if IS.null _scopesUsed then 0 else IS.findMax _scopesUsed
    (isNew, ident) <- generateIdentFor deepestScope (void e)
    topLevel <- view deepestTopLevelScope
    let qb = if deepestScope > topLevel then ByNullSourcePos else topLevelQB
    let w' = w
          & (if isNew then newBindings %~ addToScope deepestScope [(ident, (_plurality, e))] else identity)
          & plurality .~ PluralityMap (M.singleton ident False)
    pure (Var nullAnn (Qualified qb ident), w')
  (e, w) -> pure (e, w)

-- |
-- Take possession of the Binds intended to be added to the current scope,
-- removing them from the state, and return the list of Binds along with
-- whatever value is returned by the provided computation.
--
getNewBinds
  :: (HasCSEReader m, HasCSEState m, HasCSEWriter m)
  => m a
  -> m ([Bind Ann], a)
getNewBinds =
  discuss $ \(a, w) -> do
    d <- view depth
    at d .= Nothing
    let (floatedHere, w') = newBindings (popScope d) w
    pure $ first (, a) $ foldr handleFloat ([], w') floatedHere
  where
  handleFloat (ident, (p, e)) (bs, w) =
    if fromJust . M.lookup ident . getPluralityMap $ w ^. plurality
    then (NonRec nullAnn ident e : bs, w')
    else (bs, w' & toBeReinlined %~ M.insert ident e)
    where w' = w & plurality <>~ p

-- |
-- Like getNewBinds, but also stores the Binds in a Let wrapping the provided
-- expression. If said expression is already a Let, adds these Binds to that
-- Let instead.
--
getNewBindsAsLet
  :: (HasCSEReader m, HasCSEWriter m, HasCSEState m)
  => m (Expr Ann)
  -> m (Expr Ann)
getNewBindsAsLet = fmap (uncurry go) . getNewBinds where
  go bs = if null bs then identity else \case
    Let a bs' e' -> Let a (bs ++ bs') e'
    e'           -> Let nullAnn bs e'

-- |
-- Feed the Writer part of the monad with the requirements of this name.
--
summarizeName
  :: (HasCSEReader m, HasCSEWriter m)
  => ModuleName
  -> Qualified Ident
  -> m ()
summarizeName mn (Qualified mn' ident) = do
  m <- view bound
  let (s, bt) =
        fromMaybe (0, NonRecursive) $
          guard (all (== mn) (toMaybeModuleName mn')) *> ident `M.lookup` m
  tell $ mempty
       & scopesUsed .~ IS.singleton s
       & noFloatWithin .~ (guard (bt == Recursive) $> Min s)

-- |
-- Collect all the Idents put in scope by a list of Binders.
--
identsFromBinders :: [Binder a] -> [Ident]
identsFromBinders = foldMap identsFromBinder where
  identsFromBinder = \case
    LiteralBinder _ (ArrayLiteral xs)  -> identsFromBinders xs
    LiteralBinder _ (ObjectLiteral xs) -> identsFromBinders (map snd xs)
    VarBinder _ ident                  -> [ident]
    ConstructorBinder _ _ _ xs         -> identsFromBinders xs
    NamedBinder _ ident x              -> ident : identsFromBinder x
    LiteralBinder _ BooleanLiteral{}   -> []
    LiteralBinder _ CharLiteral{}      -> []
    LiteralBinder _ NumericLiteral{}   -> []
    LiteralBinder _ StringLiteral{}    -> []
    NullBinder{}                       -> []

-- |
-- Float synthetic Apps (right now, the only Apps marked as synthetic are type
-- class dictionaries being fed to functions with constraints, superclass
-- accessors, and instances of IsSymbol) to a new or existing Let as close to
-- the top level as possible.
--
optimizeCommonSubexpressions :: ModuleName -> [Bind Ann] -> Supply [Bind Ann]
optimizeCommonSubexpressions mn
  = fmap (uncurry (flip replaceLocals))
  . runCSEMonad
  . fmap (uncurry (++))
  . getNewBinds
  . fmap fst
  . handleBinds True (pure ())

  where

  -- This is the one place (I think?) that keeps this from being a general
  -- common subexpression elimination pass.
  shouldFloatExpr :: Expr Ann -> Bool
  shouldFloatExpr = \case
    App (_, _, Just IsSyntheticApp) e _ -> isSimple e
    _                                   -> False

  isSimple :: Expr Ann -> Bool
  isSimple = \case
    Var{}          -> True
    Accessor _ _ e -> isSimple e
    _              -> False

  handleAndWrapExpr :: Expr Ann -> CSEMonad (Expr Ann)
  handleAndWrapExpr = getNewBindsAsLet . handleExpr

  (handleBind, handleExprDefault, handleBinder, _) = traverseCoreFn handleBind handleExpr handleBinder handleCaseAlternative

  topLevelQB = ByModuleName mn

  handleExpr :: Expr Ann -> CSEMonad (Expr Ann)
  handleExpr = discuss (ifM (shouldFloatExpr . fst) (floatExpr topLevelQB) pure) . \case
    Abs a ident e   -> enterAbs $ Abs a ident <$> newScopeWithIdents False [ident] (handleAndWrapExpr e)
    v@(Var _ qname) -> summarizeName mn qname $> v
    Let a bs e      -> uncurry (Let a) <$> handleBinds False (handleExpr e) bs
    x               -> handleExprDefault x

  handleCaseAlternative :: CaseAlternative Ann -> CSEMonad (CaseAlternative Ann)
  handleCaseAlternative (CaseAlternative bs x) = CaseAlternative bs <$> do
    newScopeWithIdents False (identsFromBinders bs) $
      bitraverse (traverse $ bitraverse handleAndWrapExpr handleAndWrapExpr) handleAndWrapExpr x

  handleBinds :: forall a. Bool -> CSEMonad a -> [Bind Ann] -> CSEMonad ([Bind Ann], a)
  handleBinds isTopLevel = foldr go . fmap pure where
    go :: Bind Ann -> CSEMonad ([Bind Ann], a) -> CSEMonad ([Bind Ann], a)
    go b inner = case b of
      -- For a NonRec Bind, traverse the bound expression in the current scope
      -- and then create a new scope for any remaining Binds and/or whatever
      -- inner thing all these Binds are applied to.
      NonRec a ident e -> do
        e' <- handleExpr e
        newScopeWithIdents isTopLevel [ident] $
          prependToNewBindsFromInner $ NonRec a ident e'
      Rec es ->
        -- For a Rec Bind, the bound expressions need a new scope in which all
        -- these identifiers are bound recursively; then the remaining Binds
        -- and the inner thing can be traversed in the same scope with the same
        -- identifiers now bound non-recursively.
        newScope isTopLevel $ \d -> do
          let idents = map (snd . fst) es
          es' <- withBoundIdents idents (d, Recursive) $ traverse (traverse handleExpr) es
          withBoundIdents idents (d, NonRecursive) $
            prependToNewBindsFromInner $ Rec es'

      where

      prependToNewBindsFromInner :: Bind Ann -> CSEMonad ([Bind Ann], a)
      prependToNewBindsFromInner hd = first (hd :) . join <$> getNewBinds inner
