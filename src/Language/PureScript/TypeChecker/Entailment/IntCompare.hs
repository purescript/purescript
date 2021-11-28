-- |
-- Graph-based solver for comparing type-level numbers with respect to
-- reflexivity, symmetry, and transitivity properties.
--
module Language.PureScript.TypeChecker.Entailment.IntCompare where

import Protolude

import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Language.PureScript.Types as P
import qualified Language.PureScript.Constants.Prim as P

data Relation a
  = Equal a a
  | Unequal a a
  deriving (Functor, Show, Eq, Ord)

type Context a = [Relation a]

-- | An algorithm for solving relations nominally, with respect to
-- properties like reflexivity, symmetry, and transitivity.
solveRelation :: forall a. Ord a => Context a -> Relation a -> Bool
solveRelation context relation = case relation of
  -- Proving that an equality exists between two nominal values requires
  -- that if they're renamed with respect to all other equalities in the
  -- context, they're still equal.
  Equal a b ->
    rename a == rename b
  -- Proving that an inequality exists between two nominal values takes
  -- a very different code path compared to Equal; to summarize, it
  -- creates a directed graph of all inequalities in the renamed
  -- context, and searches whether a path exists from point a to point b.
  Unequal a b
    | a /= b ->
        solveInequality a b
    | otherwise ->
        False
  where
  solveInequality :: a -> a -> Bool
  solveInequality a b =
    let (graph, search) = inequalities
    in fromMaybe False $ do
      a' <- search $ rename a
      b' <- search $ rename b
      pure $ G.path graph a' b'

  alpha :: Relation a -> Relation (S.Set a)
  alpha = fmap rename

  -- Determine which "equality bucket" a value lands on, then replace
  -- that value using that bucket.
  rename :: a -> S.Set a
  rename n = fromMaybe (S.singleton n) (find (S.member n) equalities)

  -- This determines which names are equal within the context, each set
  -- in this list is a combination of all equal names, and it's built by
  -- deliberately creating a cyclic graph between the equalities so as
  -- to group them together.
  equalities :: [S.Set a]
  equalities = fmap (S.fromList . G.flattenSCC) $ makeStrong $ clean $ foldMap asNode context
    where
    asNode :: Relation a -> [(a, [a])]
    asNode = \case
      Equal a b -> [(a, [b]), (b, [a])]
      _         -> []

    makeStrong :: [(a, [a])] -> [G.SCC a]
    makeStrong = G.stronglyConnComp . fmap make

  inequalities :: (G.Graph, S.Set a -> Maybe G.Vertex)
  inequalities = asGraph $ foldMap convert context
    where
    convert :: Relation a -> [Relation (S.Set a)]
    convert = fmap alpha . \case
      c | Unequal a b <- c -> [c, Unequal b a]
      _ -> []

    asGraph :: [Relation (S.Set a)] -> (G.Graph, S.Set a -> Maybe G.Vertex)
    asGraph = makeGraph . clean . foldMap asNode
      where
      asNode :: Relation (S.Set a) -> [(S.Set a, [S.Set a])]
      asNode = \case
        (Unequal a b)    -> [(a, [b]), (b, [])]
        _                 -> []

      makeGraph :: [(S.Set a, [S.Set a])] -> (G.Graph, S.Set a -> Maybe G.Vertex)
      makeGraph m = case G.graphFromEdges $ make <$> m of
        (g, _, f) -> (g, f)

  -- Combine all key-value pairs
  clean :: forall k. Ord k => [(k, [k])] -> [(k, [k])]
  clean = M.toList . M.fromListWith (<>)

  make :: forall k. (k, [k]) -> (k, k, [k])
  make (a, b) = (a, a, b)

mkRelation :: P.Type a -> P.Type a -> P.Type a -> Maybe (Relation (P.Type a))
mkRelation lhs rhs rel = case rel of
  P.TypeConstructor _ ordering
    | ordering == P.orderingEQ -> pure $ Equal lhs rhs
    | ordering == P.orderingLT -> pure $ Unequal lhs rhs
    | ordering == P.orderingGT -> pure $ Unequal rhs lhs
  _ ->
    Nothing
