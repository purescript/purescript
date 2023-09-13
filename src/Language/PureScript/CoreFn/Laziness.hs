module Language.PureScript.CoreFn.Laziness
  ( applyLazinessTransform
  ) where

import Protolude hiding (force)
import Protolude.Unsafe (unsafeHead)

import Control.Arrow ((&&&))
import Data.Array qualified as A
import Data.Coerce (coerce)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl1', (!!))
import Data.IntMap.Monoidal qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Monoidal qualified as M
import Data.Semigroup (Max(..))
import Data.Set qualified as S

import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan(..), nullSourceSpan)
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.CoreFn (Ann, Bind, Expr(..), Literal(..), Meta(..), ssAnn, traverseCoreFn)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), InternalIdentData(..), ModuleName, Qualified(..), QualifiedBy(..), runIdent, runModuleName, toMaybeModuleName)
import Language.PureScript.PSString (mkString)

-- This module is responsible for ensuring that the bindings in recursive
-- binding groups are initialized in a valid order, introducing run-time
-- laziness and initialization checks as necessary.
--
-- PureScript is a call-by-value language with strict data constructors, this
-- transformation notwithstanding. The only laziness introduced here is in the
-- initialization of a binding. PureScript is uninterested in the order in
-- which bindings are written by the user. The compiler has always attempted to
-- emit the bindings in an order that makes sense for the backend, but without
-- this transformation, recursive bindings are emitted in an arbitrary order,
-- which can cause unexpected behavior at run time if a binding is dereferenced
-- before it has initialized.
--
-- To prevent unexpected errors, this transformation does a syntax-driven
-- analysis of a single recursive binding group to attempt to statically order
-- the bindings, and when that fails, falls back to lazy initializers that will
-- succeed or fail deterministically with a clear error at run time.
--
-- Example:
--
--   x = f \_ ->
--     x
--
-- becomes (with some details of the $runtime_lazy function elided):
-- 
--   -- the binding of x has been rewritten as a lazy initializer
--   $lazy_x = $runtime_lazy \_ ->
--     f \_ ->
--       $lazy_x 2  -- the reference to x has been rewritten as a force call
--   x = $lazy_x 1
--
-- Central to this analysis are the concepts of delay and force, which are
-- attributes given to every subexpression in the binding group. Delay and
-- force are defined by the following traversal. This traversal is used twice:
-- once to collect all the references made by each binding in the group, and
-- then again to rewrite some references to force calls. (The implications of
-- delay and force on initialization order are specified later.)

-- |
-- Visits every `Var` in an expression with the provided function, including
-- the amount of delay and force applied to that `Var`, and substitutes the
-- result back into the tree (propagating an `Applicative` effect).
--
-- Delay is a non-negative integer that represents the number of lambdas that
-- enclose an expression. Force is a non-negative integer that represents the
-- number of values that are being applied to an expression. Delay is always
-- statically determinable, but force can be *unknown*, so it's represented
-- here with a Maybe. In a function application `f a b`, `f` has force 2, but
-- `a` and `b` have unknown force--it depends on what `f` does with them.
--
-- The rules of assigning delay and force are simple:
--   * The expressions that are assigned to bindings in this group have
--     delay 0, force 0.
--   * In a function application, the function expression has force 1 higher
--     than the force of the application expression, and the argument
--     expression has unknown force.
--     * UNLESS this argument is being directly provided to a constructor (in
--       other words, the function expression is either a constructor itself or
--       a constructor that has already been partially applied), in which case
--       the force of both subexpressions is unchanged. We can assume that
--       constructors don't apply any additional force to their arguments.
--   * If the force of a lambda is zero, the delay of the body of the lambda is
--     incremented; otherwise, the force of the body of the lambda is
--     decremented. (Applying one argument to a lambda cancels out one unit of
--     delay.)
--   * In the argument of a Case and the bindings of a Let, force is unknown.
--   * Everywhere else, preserve the delay and force of the enclosing
--     expression.
--
-- Here are some illustrative examples of the above rules. We will use a
-- pseudocode syntax to annotate a subexpression with delay and force:
-- `expr#d!f` means `expr` has delay d and force f. `!*` is used to denote
-- unknown force.
--
--   x = y#0!0
--   x = y#0!2 a#0!* b#0!*
--   x = (\_ -> y#1!0)#0!0
--   x = \_ _ -> y#2!1 a#2!*
--   x = (\_ -> y#0!0)#0!1 z#0!*
--   x = Just { a: a#0!0, b: b#0!0 }
--   x = let foo = (y#1!* a b#1!*)#1!* in foo + 1
--
-- (Note that this analysis is quite ignorant of any actual control flow
-- choices made at run time. It doesn't even track what happens to a reference
-- after it has been locally bound by a Let or Case. Instead, it just assumes
-- the worst--once locally bound to a new name, it imagines that absolutely
-- anything could happen to that new name and thus to the underlying reference.
-- But the value-to-weight ratio of this approach is perhaps surprisingly
-- high.)
--
-- Every subexpression gets a delay and a force, but we are only interested
-- in references to other bindings in the binding group, so the traversal only
-- exposes `Var`s to the provided function.
--
onVarsWithDelayAndForce :: forall f. Applicative f => (Int -> Maybe Int -> Ann -> Qualified Ident -> f (Expr Ann)) -> Expr Ann -> f (Expr Ann)
onVarsWithDelayAndForce f = snd . go 0 $ Just 0
  where
  go :: Int -> Maybe Int -> (Bind Ann -> f (Bind Ann), Expr Ann -> f (Expr Ann))
  go delay force = (handleBind, handleExpr')
    where
    (handleBind, handleExpr, handleBinder, handleCaseAlternative) = traverseCoreFn handleBind handleExpr' handleBinder handleCaseAlternative
    handleExpr' = \case
      Var a i -> f delay force a i
      Abs a i e -> Abs a i <$> snd (if force == Just 0 then go (succ delay) force else go delay $ fmap pred force) e
      -- A clumsy hack to preserve TCO in a particular idiom of unsafePartial once seen in Data.Map.Internal, possibly still used elsewhere.
      App a1 e1@(Var _ C.I_unsafePartial) (Abs a2 i e2) -> App a1 e1 . Abs a2 i <$> handleExpr' e2
      App a e1 e2 ->
        -- `handleApp` is just to handle the constructor application exception
        -- somewhat gracefully (i.e., without requiring a deep inspection of
        -- the function expression at every step). If we didn't care about
        -- constructors, this could have been simply:
        --   App a <$> snd (go delay (fmap succ force)) e1 <*> snd (go delay Nothing) e2
        handleApp 1 [(a, e2)] e1
      Case a vs alts -> Case a <$> traverse (snd $ go delay Nothing) vs <*> traverse handleCaseAlternative alts
      Let a ds e -> Let a <$> traverse (fst $ go delay Nothing) ds <*> handleExpr' e
      other -> handleExpr other

    handleApp len args = \case
      App a e1 e2 -> handleApp (len + 1) ((a, e2) : args) e1
      Var a@(_, _, Just meta) i | isConstructorLike meta
        -> foldl (\e1 (a2, e2) -> App a2 <$> e1 <*> handleExpr' e2) (f delay force a i) args
      e -> foldl (\e1 (a2, e2) -> App a2 <$> e1 <*> snd (go delay Nothing) e2) (snd (go delay (fmap (+ len) force)) e) args
    isConstructorLike = \case
      IsConstructor{} -> True
      IsNewtype -> True
      _ -> False

-- Once we assign a delay and force value to every `Var` in the binding group,
-- we can consider how to order the bindings to allow them all to successfully
-- initialize. There is one principle here: each binding must be initialized
-- before the identifier being bound is ready for use. If the preorder thus
-- induced has cycles, those cycles need to be resolved with laziness. All of
-- the details concern what "ready for use" means.
--
-- The definition of delay and force suggests that "ready for use" depends on
-- those attributes. If a lambda is bound to the name x, then the references in
-- the lambda don't need to be initialized before x is initialized. This is
-- represented by the fact that those references have non-zero delay. But if
-- the expression bound to x is instead the application of a function y that is
-- also bound in this binding group, then not only does y need to be
-- initialized before x, so do some of the non-zero delay references in y. This
-- is represented by the fact that the occurrence of y in the expression bound
-- to x has non-zero force.
--
-- An example, reusing the pseudocode annotations defined above:
--
--   x _ = y#1!0
--   y = x#0!1 a
--
-- y doesn't need to be initialized before x is, because the reference to y in
-- x's initializer has delay 1. But y does need to be initialized before x is
-- ready for use with force 1, because force 1 is enough to overcome the delay
-- of that reference. And since y has a delay-0 reference to x with force 1, y
-- will need to be ready for use before it is initialized; thus, y needs to be
-- made lazy.
--
-- So just as function applications "cancel out" lambdas, a known applied force
-- cancels out an equal amount of delay, causing some references that may not
-- have been needed earlier to enter play. (And to be safe, we must assume that
-- unknown force cancels out *any* amount of delay.) There is another, subtler
-- aspect of this: if there are not enough lambdas to absorb every argument
-- applied to a function, those arguments will end up applied to the result of
-- the function. Likewise, if there is excess force left over after some of it
-- has been canceled by delay, that excess is carried to the references
-- activated. (Again, an unknown amount of force must be assumed to lead to an
-- unknown amount of excess force.)
--
-- Another example:
--
--   f = g#0!2 a b
--   g x = h#1!2 c x
--   h _ _ _ = f#3!0
--
-- Initializing f will lead to an infinite loop in this example. f invokes g
-- with two arguments. g absorbs one argument, and the second ends up being
-- applied to the result of h c x, resulting in h being invoked with three
-- arguments. Invoking h with three arguments results in dereferencing f, which
-- is not yet ready. To capture this loop in our analysis, we say that making
-- f ready for use with force 0 requires making g ready for use with force 2,
-- which requires making h ready for use with force 3 (two units of force from
-- the lexical position of h, plus one unit of excess force carried forward),
-- which cyclically requires f to be ready for use with force 0.
--
-- These preceding observations are captured and generalized by the following
-- rules:
--
--   USE-INIT: Before a reference to x is ready for use with any force, x must
--     be initialized.
--
--     We will make x lazy iff this rule induces a cycle--i.e., initializing x
--     requires x to be ready for use first.
--
--   USE-USE: Before a reference to x is ready for use with force f:
--     * if a reference in the initializer of x has delay d and force f',
--     * and either d <= f or f is unknown,
--     * then that reference must itself be ready for use with
--       force f – d + f' (or with unknown force if f or f' is unknown).
--
--   USE-IMMEDIATE: Initializing a binding x is equivalent to requiring a
--     reference to x to be ready for use with force 0, per USE-USE.
--     
--     Equivalently: before x is initialized, any reference in the initializer
--     of x with delay 0 and force f must be ready for use with force f.
--
-- Examples:
--
--   Assume x is bound in a recursive binding group with the below bindings.
--
--   All of the following initializers require x to be ready for use with some
--   amount of force, and therefore require x to be initialized first.
--
--   a = x#0!0
--   b = (\_ -> x#0!0) 1
--   c = foo x#0!*
--   d = (\_ -> foo x#0!*) 1
--
--   In the following initializers, before p can be initialized, x must be
--   ready for use with force f – d + f'. (And both x and q must be
--   initialized, of course; but x being ready for use with that force may
--   induce additional constraints.)
--
--   p = ... q#0!f ...
--   q = ... x#d!f' ... (where d <= f)
--
--   Excess force stacks, of course: in the following initializers, before r
--   can be initialized, x must be ready for use with force
--   f — d + f' — d' + f'':
--
--   r = ... s#0!f ...
--   s = ... t#d!f' ... (where d <= f)
--   t = ... x#d'!f'' ... (where d' <= f – d + f')
--
--
-- To satisfy these rules, we will construct a graph between (identifier,
-- delay) pairs, with edges induced by the USE-USE rule, and effectively run a
-- topsort to get the initialization preorder. For this part, it's simplest to
-- think of delay as an element of the naturals extended with a positive
-- infinity, corresponding to an unknown amount of force. (We'll do arithmetic
-- on these extended naturals as you would naively expect; we won't do anything
-- suspect like subtracting infinity from infinity.) With that in mind, we can
-- construct the graph as follows: for each reference from i1 to i2 with delay
-- d and force f, draw an infinite family of edges from (i1, d + n) to (i2, f +
-- n) for all 0 <= n <= ∞, where n represents the excess force carried over
-- from a previous edge. Unfortunately, as an infinite graph, we can't expect
-- the tools in Data.Graph to help us traverse it; we will have to be a little
-- bit clever.
--
-- The following data types and functions are for searching this infinite graph
-- and carving from it a finite amount of data to work with. Specifically, we
-- want to know for each identifier i, which other identifiers are
-- irreflexively reachable from (i, 0) (and thus must be initialized before i
-- is), and with what maximum force (in the event of a loop, not every
-- reference to i in the reachable identifier needs to be rewritten to a force
-- call; only the ones with delay up to the maximum force used during i's
-- initialization). We also want the option of aborting a given reachability
-- search, for one of two reasons.
--
--   * If we encounter a reference with unknown force, abort.
--   * If we encounter a cycle where force on a single identifier is
--     increasing, abort. (Because of USE-USE, as soon as an identifier is
--     revisited with greater force than its first visit, the difference is
--     carried forward as excess, so it is possible to retrace that path to get
--     an arbitrarily high amount of force.)
--
-- Both reasons mean that it is theoretically possible for the identifier in
-- question to need every other identifier in the binding group to be
-- initialized before it is. (Every identifier in a recursive binding group is
-- necessarily reachable from every other, ignoring delay and force, which is
-- what arbitrarily high force lets you do.)
--
-- In order to reuse parts of this reachability computation across identifiers,
-- we are going to represent it with a rose tree data structure interleaved with
-- a monad capturing the abort semantics. (The monad is Maybe, but we don't
-- need to know that here!)

type MaxRoseTree m a = m (IM.MonoidalIntMap (MaxRoseNode m a))
data MaxRoseNode m a = MaxRoseNode a (MaxRoseTree m a)

-- Dissecting this data structure:
--
-- m (...)
-- ^ represents whether to abort or continue the search
--
--   IM.MonoidalIntMap (...)
--   ^ the keys of this map are other identifiers reachable from the current
--     one (we'll map the identifiers in this binding group to Ints for ease of
--     computation)
--
--     the values of this map are:
--
--     MaxRoseNode a (...)
--     ^ this will store the force applied to the next identifier
--       (MaxRoseTree m a)
--       ^ and this, the tree of identifiers reachable from there
--
-- We're only interested in continuing down the search path that applies the
-- most force to a given identifier! So when we combine two MaxRoseTrees,
-- we want to resolve any key collisions in their MonoidalIntMaps with this
-- semigroup:

instance Ord a => Semigroup (MaxRoseNode m a) where
  l@(MaxRoseNode l1 _) <> r@(MaxRoseNode r1 _) = if r1 > l1 then r else l

-- And that's why this is called a MaxRoseTree.
--
-- Traversing this tree to get a single MonoidalIntMap with the entire closure
-- plus force information is fairly straightforward:

mrtFlatten :: (Monad m, Ord a) => MaxRoseTree m a -> m (IM.MonoidalIntMap (Max a))
mrtFlatten = (getAp . IM.foldMapWithKey (\i (MaxRoseNode a inner) -> Ap $ (IM.singleton i (Max a) <>) <$> mrtFlatten inner) =<<)

-- The use of the `Ap` monoid ensures that if any child of this tree aborts,
-- the entire tree aborts.
--
-- One might ask, why interleave the abort monad with the tree at all if we're
-- just going to flatten it out at the end? The point is to flatten it out at
-- the end, but *not* during the generation of the tree. Attempting to flatten
-- the tree as we generate it can result in an infinite loop, because a subtree
-- needs to be exhaustively searched for abort conditions before it can be used
-- in another tree. With this approach, we can use lazy trees as building
-- blocks and, as long as they get rewritten to be finite or have aborts before
-- they're flattened, the analysis still terminates.

-- |
-- Given a maximum index and a function that returns a map of edges to next
-- indices, returns an array for each index up to maxIndex of maps from the
-- indices reachable from the current index, to the maximum force applied to
-- those indices.
searchReachable
  :: forall m force
   . (Alternative m, Monad m, Enum force, Ord force)
  => Int
  -> ((Int, force) -> m (IM.MonoidalIntMap (Max force)))
  -> A.Array Int (m (IM.MonoidalIntMap (Max force)))
searchReachable maxIdx lookupEdges = mrtFlatten . unsafeHead <$> mem
  where
  -- This is a finite array of infinite lists, used to memoize all the search
  -- trees. `unsafeHead` is used above to pull the first tree out of each list
  -- in the array--the one corresponding to zero force, which is what's needed
  -- to initialize the corresponding identifier. (`unsafeHead` is safe here, of
  -- course: infinite lists.)
  mem :: A.Array Int [MaxRoseTree m force]
  mem = A.listArray (0, maxIdx)
    [ [cutLoops <*> fmap (IM.mapWithKey memoizedNode) . lookupEdges $ (i, f) | f <- [toEnum 0..]]
    | i <- [0..maxIdx]
    ]

  memoizedNode :: Int -> Max force -> MaxRoseNode m force
  memoizedNode i (Max force) = MaxRoseNode force $ mem A.! i !! fromEnum force

  -- And this is the function that prevents the search from actually being
  -- infinite. It applies a filter to a `MaxRoseTree` at every level, looking for
  -- indices anywhere in the tree that match the current vertex. If a match is
  -- found with greater force than the current force, that part of the tree is
  -- rewritten to abort; otherwise, that part of the tree is rewritten to be
  -- empty (there's nothing new in that part of the search).
  --
  -- A new version of `cutLoops` is applied for each node in the search, so
  -- each edge in a search path will add another filter on a new index. Since
  -- there are a finite number of indices in our universe, this guarantees that
  -- the analysis terminates, because no single search path can have length
  -- greater than `maxIdx`.
  cutLoops :: (Int, force) -> MaxRoseTree m force -> MaxRoseTree m force
  cutLoops (i, force) = go
    where
    go = (=<<) . IM.traverseWithKey $ \i' (MaxRoseNode force' inner) ->
      MaxRoseNode force' <$> if i == i' then guard (force >= force') $> pure IM.empty else pure $ go inner

-- One last data structure to define and then it's on to the main event.
--
-- The laziness transform effectively takes a list of eager bindings (x = ...)
-- and splits some of them into lazy definitions ($lazy_x = ...) and lazy
-- bindings (x = $lazy_x ...). It's convenient to work with these three
-- declarations as the following sum type:

data RecursiveGroupItem e = EagerBinding Ann e | LazyDefinition e | LazyBinding Ann
  deriving Functor

-- |
-- Transform a recursive binding group, reordering the bindings within when a
-- correct initialization order can be statically determined, and rewriting
-- bindings and references to be lazy otherwise.
--
applyLazinessTransform :: ModuleName -> [((Ann, Ident), Expr Ann)] -> ([((Ann, Ident), Expr Ann)], Any)
applyLazinessTransform mn rawItems = let

  -- Establish the mapping from names to ints.
  rawItemsByName :: M.MonoidalMap Ident (Ann, Expr Ann)
  rawItemsByName = M.fromList $ (snd . fst &&& first fst) <$> rawItems

  maxIdx = M.size rawItemsByName - 1

  rawItemsByIndex :: A.Array Int (Ann, Expr Ann)
  rawItemsByIndex = A.listArray (0, maxIdx) $ M.elems rawItemsByName

  names :: S.Set Ident
  names = M.keysSet rawItemsByName

  -- Now do the first delay/force traversal of all the bindings to find
  -- references to other names in this binding group.
  --
  -- The parts of this type mean:
  -- D is the maximum force (or Nothing if unknown) with which the identifier C
  -- is referenced in any delay-B position inside the expression A.
  --
  -- where A, B, C, and D are as below:
  --                A           B (keys)           C (keys)           D
  findReferences :: Expr Ann -> IM.MonoidalIntMap (IM.MonoidalIntMap (Ap Maybe (Max Int)))
  findReferences = (getConst .) . onVarsWithDelayAndForce $ \delay force _ -> \case
    Qualified qb ident | all (== mn) (toMaybeModuleName qb), Just i <- ident `S.lookupIndex` names
      -> Const . IM.singleton delay . IM.singleton i $ coerceForce force
    _ -> Const IM.empty

  -- The parts of this type mean:
  -- D is the maximum force (or Nothing if unknown) with which the identifier C
  -- is referenced in any delay-B position inside the binding of identifier A.
  --
  -- where A, B, C, and D are as below:
  --                     A    B (keys)           C (keys)           D
  refsByIndex :: A.Array Int (IM.MonoidalIntMap (IM.MonoidalIntMap (Ap Maybe (Max Int))))
  refsByIndex = findReferences . snd <$> rawItemsByIndex

  -- Using the approach explained above, traverse the reference graph generated
  -- by `refsByIndex` and find all reachable names.
  --
  -- The parts of this type mean:
  -- D is the maximum force with which the identifier C is referenced,
  -- directly or indirectly, during the initialization of identifier A. B is
  -- Nothing if the analysis of A was inconclusive and A might need the entire
  -- binding group.
  -- 
  -- where A, B, C, and D are as below:
  --                           A    B      C (keys)           D
  reachablesByIndex :: A.Array Int (Maybe (IM.MonoidalIntMap (Max Int)))
  reachablesByIndex = searchReachable maxIdx $ \(i, force) ->
    getAp . flip IM.foldMapWithKey (dropKeysAbove force $ refsByIndex A.! i) $ \delay ->
      IM.foldMapWithKey $ \i' force' ->
        Ap $ IM.singleton i' . Max . (force - delay +) <$> uncoerceForce force'

  -- If `reachablesByIndex` is a sort of labeled relation, this function
  -- produces part of the reverse relation, but only for the edges from the
  -- given vertex.
  --
  -- The parts of this type mean:
  -- The identifier A is reachable from the identifier B with maximum force C
  -- (B is also the index provided to the function).
  --
  -- where A, B, and C are as below:
  --                      (B)    A                  B (singleton key)  C
  reverseReachablesFor :: Int -> IM.MonoidalIntMap (IM.MonoidalIntMap (Ap Maybe (Max Int)))
  reverseReachablesFor i = case reachablesByIndex A.! i of
    Nothing -> IM.fromAscList $ (, IM.singleton i $ Ap Nothing) <$> [0..maxIdx]
    Just im -> IM.singleton i . Ap . Just <$> im

  -- We can use `reachablesByIndex` to build a finite graph and topsort it;
  -- in the process, we'll pack the nodes of the graph with data we'll want
  -- next. Remember that if our reachability computation aborted, we have to
  -- assume that every other identifier is reachable from that one--hence the
  -- `maybe [0..maxIdx]`.
  sccs = stronglyConnComp $ do
    (i, mbReachable) <- A.assocs reachablesByIndex
    pure ((reverseReachablesFor i, (S.elemAt i names, rawItemsByIndex A.! i)), i, maybe [0..maxIdx] (IS.toList . IM.keysSet) mbReachable)

  (replacements, items) = flip foldMap sccs $ \case
    -- The easy case: this binding doesn't need to be made lazy after all!
    AcyclicSCC (_, (ident, (a, e))) -> pure [(ident, EagerBinding a e)]
    -- The tough case: we have a loop.
    -- We need to do two things here:
    --   * Collect the reversed reachables relation for each vertex in this
    --     loop; we'll use this to replace references with force calls
    --   * Copy the vertex list into two lists: a list of lazy definitions and
    --     a list of lazy bindings
    -- Both of these results are monoidal, so the outer `foldMap` will
    -- concatenate them pairwise.
    CyclicSCC vertices -> (foldMap fst vertices, map (fmap (LazyDefinition . snd) . snd) vertices ++ map (fmap (LazyBinding . fst) . snd) vertices)

  -- We have `replacements` expressed in terms of indices; we want to map it
  -- back to names before traversing the bindings again.
  replacementsByName :: M.MonoidalMap Ident (M.MonoidalMap Ident (Ap Maybe (Max Int)))
  replacementsByName = M.fromAscList . map (bimap (flip S.elemAt names) (M.fromAscList . map (first (flip S.elemAt names)) . IM.toAscList)) . IM.toAscList $ replacements

  -- And finally, this is the second delay/force traversal where we take
  -- `replacementsByName` and use it to rewrite references with force calls,
  -- but only if the delay of those references is at most the maximum amount
  -- of force used by the initialization of the referenced binding to
  -- reference the outer binding. A reference made with a higher delay than
  -- that can safely continue to use the original reference, since it won't be
  -- needed until after the referenced binding is done initializing.
  replaceReferencesWithForceCall :: (Ident, RecursiveGroupItem (Expr Ann)) -> (Ident, RecursiveGroupItem (Expr Ann))
  replaceReferencesWithForceCall pair@(ident, item) = case ident `M.lookup` replacementsByName of
    Nothing -> pair
    Just m -> let
      rewriteExpr = (runIdentity .) . onVarsWithDelayAndForce $ \delay _ ann -> pure . \case
        Qualified qb ident' | all (== mn) (toMaybeModuleName qb), any (all (>= Max delay) . getAp) $ ident' `M.lookup` m
          -> makeForceCall ann ident'
        q -> Var ann q
      in (ident, rewriteExpr <$> item)

  -- All that's left to do is run the above replacement on every item,
  -- translate items from our `RecursiveGroupItem` representation back into the
  -- form CoreFn expects, and inform the caller whether we made any laziness
  -- transformations after all. (That last bit of information is used to
  -- determine if the runtime factory function needs to be injected.)
  in (uncurry fromRGI . replaceReferencesWithForceCall <$> items, Any . not $ IM.null replacements)

  where

  nullAnn = ssAnn nullSourceSpan
  runtimeLazy = Var nullAnn . Qualified ByNullSourcePos $ InternalIdent RuntimeLazyFactory
  runFn3 = Var nullAnn . Qualified (ByModuleName C.M_Data_Function_Uncurried) . Ident $ C.S_runFn <> "3"
  strLit = Literal nullAnn . StringLiteral . mkString

  lazifyIdent = \case
    Ident txt -> InternalIdent $ Lazy txt
    _ -> internalError "Unexpected argument to lazifyIdent"

  makeForceCall :: Ann -> Ident -> Expr Ann
  makeForceCall (ss, _, _) ident
    -- We expect the functions produced by `runtimeLazy` to accept one
    -- argument: the line number on which this reference is made. The runtime
    -- code uses this number to generate a message that identifies where the
    -- evaluation looped.
    = App nullAnn (Var nullAnn . Qualified ByNullSourcePos $ lazifyIdent ident)
    . Literal nullAnn . NumericLiteral . Left . toInteger . sourcePosLine
    $ spanStart ss

  fromRGI :: Ident -> RecursiveGroupItem (Expr Ann) -> ((Ann, Ident), Expr Ann)
  fromRGI i = \case
    EagerBinding a e -> ((a, i), e)
    -- We expect the `runtimeLazy` factory to accept three arguments: the
    -- identifier being initialized, the name of the module, and of course a
    -- thunk that actually contains the initialization code.
    LazyDefinition e -> ((nullAnn, lazifyIdent i), foldl1' (App nullAnn) [runFn3, runtimeLazy, strLit $ runIdent i, strLit $ runModuleName mn, Abs nullAnn UnusedIdent e])
    LazyBinding a -> ((a, i), makeForceCall a i)

  dropKeysAbove :: Int -> IM.MonoidalIntMap a -> IM.MonoidalIntMap a
  dropKeysAbove n = fst . IM.split (n + 1)

  coerceForce :: Maybe Int -> Ap Maybe (Max Int)
  coerceForce = coerce

  uncoerceForce :: Ap Maybe (Max Int) -> Maybe Int
  uncoerceForce = coerce
