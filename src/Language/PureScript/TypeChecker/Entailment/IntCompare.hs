-- |
-- Graph-based solver for comparing type-level numbers with respect to
-- reflexivity, symmetry, and transitivity properties.
--
module Language.PureScript.TypeChecker.Entailment.IntCompare where

import Protolude

import Control.Arrow ((&&&))
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Language.PureScript.Types as P
import qualified Language.PureScript.Constants.Prim as P

data Comparison a
  = Equal a a
  | LessThan a a
  | GreaterThan a a
  deriving (Show, Eq, Ord)

(.=.) :: a -> a -> Comparison a
(.=.) = Equal

(.<.) :: a -> a -> Comparison a
(.<.) = LessThan

(.>.) :: a -> a -> Comparison a
(.>.) = GreaterThan

type Context a = [Comparison a]

solveComparison :: forall a. Ord a => Context a -> Comparison a -> Bool
solveComparison context comparison = case comparison of
    Equal a b
      | a == b -> True
      | otherwise ->
          let
            comparison' = alpha comparison
            context' = alpha <$> context
          in
            comparison' `elem` context'
    LessThan a b ->
      solveInequality fst a b
    GreaterThan a b ->
      solveInequality snd a b
  where
  solveInequality
    :: ( ( (G.Graph, S.Set a -> Maybe G.Vertex)
         , (G.Graph, S.Set a -> Maybe G.Vertex)
         ) -> (G.Graph, S.Set a -> Maybe G.Vertex)
       )
    -> a
    -> a
    -> Bool
  solveInequality f a b =
    let
      (graph, search) = f inequalities
      a' = rename a
      b' = rename b
    in fromMaybe False $ do
      norm <- G.path graph <$> search a' <*> search b'
      symm <- G.path (G.transposeG graph) <$> search b' <*> search a'
      pure $ norm || symm

  alpha :: Comparison a -> Comparison (S.Set a)
  alpha = \case
    Equal a b       -> Equal (rename a) (rename b)
    LessThan a b    -> LessThan (rename a) (rename b)
    GreaterThan a b -> GreaterThan (rename a) (rename b)

  rename :: a -> S.Set a
  rename n = fromMaybe (S.singleton n) (find (S.member n) equalities)

  equalities :: [S.Set a]
  equalities = fmap (S.fromList . G.flattenSCC) $ makeStrong $ clean $ foldMap asNode context
    where
    asNode :: Comparison a -> [(a, [a])]
    asNode = \case
      Equal a b -> [(a, [b]), (b, [a])]
      _         -> []

    makeStrong :: [(a, [a])] -> [G.SCC a]
    makeStrong = G.stronglyConnComp . fmap make

  inequalities ::
    ( ( G.Graph, (S.Set a) -> Maybe G.Vertex )
    , ( G.Graph, (S.Set a) -> Maybe G.Vertex )
    )
  inequalities = lts &&& gts $ foldMap convert context
    where
    convert :: Comparison a -> [Comparison (S.Set a)]
    convert = fmap alpha . \case
      c | LessThan a b <- c -> [c, GreaterThan b a]
        | GreaterThan a b <- c -> [c, LessThan b a]
      _ -> []

    asGraph :: [Comparison (S.Set a)] -> (G.Graph, (S.Set a) -> Maybe G.Vertex)
    asGraph = makeGraph . clean . foldMap asNode
      where
      asNode :: Comparison (S.Set a) -> [(S.Set a, [S.Set a])]
      asNode (LessThan a b)    = [(a, [b]), (b, [])]
      asNode (GreaterThan a b) = [(a, [b]), (b, [])]
      asNode _                 = []

      makeGraph :: [(S.Set a, [S.Set a])] -> (G.Graph, S.Set a -> Maybe G.Vertex)
      makeGraph m = case G.graphFromEdges $ make <$> m of
        (g, _, f) -> (g, f)

    lts :: [Comparison (S.Set a)] -> (G.Graph, S.Set a -> Maybe G.Vertex)
    lts = asGraph . go where go n = [LessThan a b | LessThan a b <- n]

    gts :: [Comparison (S.Set a)] -> (G.Graph, S.Set a -> Maybe G.Vertex)
    gts = asGraph . go where go n = [GreaterThan a b | GreaterThan a b <- n]

  clean :: forall k. Ord k => [(k, [k])] -> [(k, [k])]
  clean = M.toList . M.fromListWith (<>)

  make :: forall k. (k, [k]) -> (k, k, [k])
  make (a, b) = (a, a, b)

mkComparison :: P.Type a -> P.Type a -> P.Type a -> Maybe (Comparison (P.Type a))
mkComparison lhs rhs cmp = case cmp of
  P.TypeConstructor _ ordering
    | ordering == P.orderingEQ -> pure $ lhs .=. rhs
    | ordering == P.orderingLT -> pure $ lhs .<. rhs
    | ordering == P.orderingGT -> pure $ lhs .>. rhs
  _ ->
    Nothing
