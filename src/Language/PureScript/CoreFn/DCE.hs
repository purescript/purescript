-- Dead Code Elimination for CoreFn
module Language.PureScript.CoreFn.DCE
  ( dce ) where

import           Prelude.Compat
import           Control.Monad
import           Data.Graph
import           Data.List (any, elem, filter, groupBy, sortBy)
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

import           Language.PureScript.CoreFn
import           Language.PureScript.Names

type Key = Qualified Ident

dce :: forall t a. [ModuleT t a] -> [Qualified Ident] -> [ModuleT t a]
dce modules [] = modules
dce modules entryPoints = do
    vs <- reachableList
    Module {..} <- modules
    guard (getModuleName vs == Just moduleName)
    let
        -- | filter declarations preserving the same order
        decls :: [Bind a]
        decls = filter filterByIdents moduleDecls
          where
          declIdents :: [Ident]
          declIdents = concatMap (\(b, _, _) -> bindIdents b) vs

          filterByIdents :: Bind a -> Bool
          filterByIdents = any (`elem` declIdents) . bindIdents

        idents :: [Ident]
        idents = concatMap getBindIdents decls

        exports :: [Ident]
        exports = filter (`elem` idents) moduleExports

        mods :: [ModuleName]
        mods = mapMaybe getQual (concatMap (\(_, _, ks) -> ks) vs)

        imports :: [(a, ModuleName)]
        imports = filter ((`elem` mods) . snd) moduleImports

        foreigns :: [ForeignDeclT t]
        foreigns = filter (\(i, _) -> mkQualified i moduleName `S.member` reachableSet) moduleForeign
          where
            reachableSet = foldr (\(_, k, ks) s -> S.insert k s `S.union` S.fromList ks)  S.empty vs

    return $ Module moduleComments moduleName imports exports foreigns decls
  where
  (graph, keyForVertex, vertexForKey) = graphFromEdges verts

  bindIdents :: Bind a -> [Ident]
  bindIdents (NonRec _ i _) = [i]
  bindIdents (Rec l) = map (\((_, i), _) -> i) l

  -- | The Vertex set
  verts :: [(Bind a, Key, [Key])]
  verts = do
      Module _ mn _ _ _ ds <- modules
      concatMap (toVertices mn) ds
    where
    toVertices :: ModuleName -> Bind a -> [(Bind a, Key, [Key])]
    toVertices mn b@(NonRec _ i e) = [(b, mkQualified i mn, deps e)]
    toVertices mn b@(Rec bs) = 
      -- assuming that `Constructor` expressions are not defined in a recursive
      -- binding
      let ks :: [(Key, [Key])]
          ks = map (\((_, i), e) -> (mkQualified i mn, deps e)) bs
      in map (\(k, ks') -> (b, k, map fst ks ++ ks')) ks

    -- | Find dependencies of an expression
    deps :: Expr a -> [Key]
    deps = go
      where
        (_, go, _, _) = everythingOnValues (++)
          (const [])
          onExpr
          onBinder
          (const [])

        -- | Build graph only from qualified identifiers
        onExpr :: Expr a -> [Key]
        onExpr (Var _ i) = if isQualified i then [i] else []
        onExpr _ = []

        onBinder :: Binder a -> [Key]
        onBinder (ConstructorBinder _ _ c _) = [fmap (Ident . runProperName) c]
        onBinder _ = []

  -- | Vertices corresponding to the entry points which we want to keep.
  entryPointVertices :: [Vertex]
  entryPointVertices = catMaybes $ do
    (_, k, _) <- verts
    guard $ k `elem` entryPoints
    return (vertexForKey k)

  -- | The list of reachable vertices grouped by module name
  reachableList :: [[(Bind a, Key, [Key])]]
  reachableList
    = groupBy (\(_, k1, _) (_, k2, _) -> getQual k1 == getQual k2)
    $ sortBy (\(_, k1, _) (_, k2, _) -> getQual k1 `compare` getQual k2) 
    $ map keyForVertex (concatMap (reachable graph) entryPointVertices)

  getModuleName :: [(Bind a, Key, [Key])] -> Maybe ModuleName
  getModuleName [] = Nothing
  getModuleName ((_, k, _) : _) = getQual k

  getBindIdents :: Bind a -> [Ident]
  getBindIdents (NonRec _ i _) = [i]
  getBindIdents (Rec is) = map (\((_, i), _) -> i) is

dceLetExpr :: Expr a -> Expr a
dceLetExpr (Let a bs e) = Let a bs e
dceLetExpr e = e
