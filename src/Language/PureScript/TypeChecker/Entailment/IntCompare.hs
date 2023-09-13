-- |
-- Graph-based solver for comparing type-level numbers with respect to
-- reflexivity, symmetry, and transitivity properties.
--
module Language.PureScript.TypeChecker.Entailment.IntCompare where

import Protolude

import Data.Graph qualified as G
import Data.Map qualified as M

import Language.PureScript.Names qualified as P
import Language.PureScript.Types qualified as P
import Language.PureScript.Constants.Prim qualified as P

data Relation a
  = Equal a a
  | LessThan a a
  deriving (Functor, Show, Eq, Ord)

type Context a = [Relation a]

type PSOrdering = P.Qualified (P.ProperName 'P.TypeName)

-- Commentary:
--
-- In essence, this solver builds a directed graph using the provided
-- context, which is then used to determine the relationship between
-- the two elements being compared.
--
-- Given the context [a < b, b < c], we can infer that a < c as a
-- path exists from a to c. Likewise, we can also infer that c > a
-- as a path exists from c to a.
--
-- ╔═══╗    ╔═══╗    ╔═══╗
-- ║ a ║ -> ║ b ║ -> ║ c ║
-- ╚═══╝    ╚═══╝    ╚═══╝
--
-- Introducing equality to the context augments the graph further,
-- and it is represented by creating cycles between equal nodes.
-- For example, [a < b, b < c, c = d] yields the following graph:
--
-- ╔═══╗    ╔═══╗    ╔═══╗     ╔═══╗
-- ║ a ║ -> ║ b ║ -> ║ c ║ <-> ║ d ║
-- ╚═══╝    ╚═══╝    ╚═══╝     ╚═══╝
solveRelation :: forall a. Ord a => Context a -> a -> a -> Maybe PSOrdering
solveRelation context lhs rhs =
  if lhs == rhs then
    pure P.EQ
  else do
    let (graph, search) = inequalities
    lhs' <- search lhs
    rhs' <- search rhs
    case (G.path graph lhs' rhs', G.path graph rhs' lhs') of
      (True, True) ->
        pure P.EQ
      (True, False) ->
        pure P.LT
      (False, True) ->
        pure P.GT
      _ ->
        Nothing
  where
  inequalities :: (G.Graph, a -> Maybe G.Vertex)
  inequalities = makeGraph $ clean $ foldMap convert context
    where
    convert :: Relation a -> [(a, [a])]
    convert (Equal a b)    = [(a, [b]), (b, [a])]
    convert (LessThan a b) = [(a, [b]), (b, [])]

    makeGraph :: [(a, [a])] -> (G.Graph, a -> Maybe G.Vertex)
    makeGraph m =
      case G.graphFromEdges $ (\(a, b) -> (a, a, b)) <$> m of
        (g, _, f) -> (g, f)

    clean :: forall k. Ord k => [(k, [k])] -> [(k, [k])]
    clean = M.toList . M.fromListWith (<>)

mkRelation :: P.Type a -> P.Type a -> P.Type a -> Maybe (Relation (P.Type a))
mkRelation lhs rhs rel = case rel of
  P.TypeConstructor _ ordering
    | ordering == P.EQ -> pure $ Equal lhs rhs
    | ordering == P.LT -> pure $ LessThan lhs rhs
    | ordering == P.GT -> pure $ LessThan rhs lhs
  _ ->
    Nothing

mkFacts :: [[P.Type a]] -> [Relation (P.Type a)]
mkFacts = mkRels [] . sort . findFacts
  where
  mkRels a [] = concat a
  mkRels a (x : xs) = mkRels (map (LessThan x) xs : a) xs

  findFacts = mapMaybe $ \case
    [P.TypeLevelInt _ _, P.TypeLevelInt _ _, _] ->
      Nothing
    [i@(P.TypeLevelInt _ _), _, _] ->
      Just i
    [_, i@(P.TypeLevelInt _ _), _] ->
      Just i
    _ ->
      Nothing
