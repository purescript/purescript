-- Dead Code Elimination for CoreFn
module Language.PureScript.DCE
  ( dce
  , dceForeignModule
  ) where

import           Prelude.Compat
import           Control.Monad
import           Data.Graph
import           Data.List (any, elem, filter, groupBy, sortBy)
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import           Language.JavaScript.Parser.AST (JSStatement(..), JSExpression(..))

import           Language.PureScript.CoreFn
import           Language.PureScript.Names

type Key = Qualified Ident

data DCEVertex a
  = BindVertex (Bind a)
  | ForeignVertex (Qualified Ident)

dce :: forall t a. [ModuleT t a] -> [Qualified Ident] -> [ModuleT t a]
dce modules [] = modules
dce modules entryPoints = do
    vs <- reachableList
    Module {..} <- modules
    guard (getModuleName vs == Just moduleName)
    let
        -- | filter declarations preserving the order
        decls :: [Bind a]
        decls = filter filterByIdents moduleDecls
          where
          declIdents :: [Ident]
          declIdents = concatMap toIdents vs

          toIdents :: (DCEVertex a, Key, [Key]) -> [Ident]
          toIdents (BindVertex b, _, _) = bindIdents b
          toIdents _                    = []

          filterByIdents :: Bind a -> Bool
          filterByIdents = any (`elem` declIdents) . bindIdents

        idents :: [Ident]
        idents = concatMap getBindIdents decls

        exports :: [Ident]
        exports = filter (`elem` (idents ++ fst `map` foreigns)) moduleExports

        mods :: [ModuleName]
        mods = mapMaybe getQual (concatMap (\(_, _, ks) -> ks) vs)

        imports :: [(a, ModuleName)]
        imports = filter ((`elem` mods) . snd) moduleImports

        foreigns :: [ForeignDeclT t]
        foreigns = filter ((`S.member` reachableSet) . Qualified (Just moduleName) . fst) moduleForeign
          where
            reachableSet = foldr (\(_, k, ks) s -> S.insert k s `S.union` S.fromList ks) S.empty vs

    return $ Module moduleComments moduleName imports exports foreigns decls
  where
  (graph, keyForVertex, vertexForKey) = graphFromEdges verts

  bindIdents :: Bind a -> [Ident]
  bindIdents (NonRec _ i _) = [i]
  bindIdents (Rec l) = map (\((_, i), _) -> i) l

  -- | The Vertex set
  verts :: [(DCEVertex a, Key, [Key])]
  verts = do
      Module _ mn _ _ mf ds <- modules
      concatMap (toVertices mn) ds ++ ((\q -> (ForeignVertex q, q, [])) . flip mkQualified mn . fst) `map` mf
    where
    toVertices :: ModuleName -> Bind a -> [(DCEVertex a, Key, [Key])]
    toVertices mn b@(NonRec _ i e) = [(BindVertex b, mkQualified i mn, deps e)]
    toVertices mn b@(Rec bs) = 
      -- assuming that `Constructor` expressions are not defined in a recursive
      -- binding
      let ks :: [(Key, [Key])]
          ks = map (\((_, i), e) -> (mkQualified i mn, deps e)) bs
      in map (\(k, ks') -> (BindVertex b, k, map fst ks ++ ks')) ks

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
  reachableList :: [[(DCEVertex a, Key, [Key])]]
  reachableList
    = groupBy (\(_, k1, _) (_, k2, _) -> getQual k1 == getQual k2)
    $ sortBy (\(_, k1, _) (_, k2, _) -> getQual k1 `compare` getQual k2) 
    $ map keyForVertex (concatMap (reachable graph) entryPointVertices)

  getModuleName :: [(DCEVertex a, Key, [Key])] -> Maybe ModuleName
  getModuleName [] = Nothing
  getModuleName ((_, k, _) : _) = getQual k

  getBindIdents :: Bind a -> [Ident]
  getBindIdents (NonRec _ i _) = [i]
  getBindIdents (Rec is) = map (\((_, i), _) -> i) is

-- | Filter export statements in foreign module.  This is not safe.  It might
-- remove declarations that are used eleswhere in the foreign module.
dceForeignModule :: [Ident] -> [JSStatement] -> [JSStatement]
dceForeignModule is ss = filter filterExports ss
  where
  ns = T.unpack . runIdent <$> is

  filterExports :: JSStatement -> Bool
  filterExports (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ x)) _ _ _) = x `elem` ns
  filterExports _ = True
