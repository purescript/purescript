-- |
-- Graph-based solver for comparing type-level numbers with respect to
-- reflexivity, symmetry, and transitivity properties.
--
module Language.PureScript.TypeChecker.Entailment.IntCompare where

import Protolude

import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Types as P
import qualified Language.PureScript.Constants.Prim as P

data Relation a
  = Equal a a
  | Unequal a a
  deriving (Functor, Show, Eq, Ord)

type Context a = [Relation a]

type PSOrdering = P.Qualified (P.ProperName 'P.TypeName)

solveRelation :: forall a. Ord a => Context a -> a -> a -> Maybe PSOrdering
solveRelation context lhs rhs =
  let
    lhs' = rename lhs
    rhs' = rename rhs
  in
    if lhs' == rhs' then
      pure P.orderingEQ
    else if solveInequality lhs' rhs' then
      pure P.orderingLT
    else if solveInequality rhs' lhs' then
      pure P.orderingGT
    else
      Nothing
  where
  solveInequality :: S.Set a -> S.Set a -> Bool
  solveInequality a b =
    let (graph, search) = inequalities
    in fromMaybe False $ do
      a' <- search a
      b' <- search b
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
    asNode (Equal a b) = [(a, [b]), (b, [a])]
    asNode _           = mempty

    makeStrong :: [(a, [a])] -> [G.SCC a]
    makeStrong = G.stronglyConnComp . fmap make

  inequalities :: (G.Graph, S.Set a -> Maybe G.Vertex)
  inequalities = asGraph $ foldMap convert context
    where
    convert :: Relation a -> [Relation (S.Set a)]
    convert (Unequal a b) = pure $ alpha (Unequal a b)
    convert _             = mempty

    asGraph :: [Relation (S.Set a)] -> (G.Graph, S.Set a -> Maybe G.Vertex)
    asGraph = makeGraph . clean . foldMap asNode
      where
      asNode :: Relation (S.Set a) -> [(S.Set a, [S.Set a])]
      asNode (Unequal a b) = [(a, [b]), (b, [])]
      asNode _             = mempty

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
