-- Dead Code Elimination for CoreFn
module Language.PureScript.DCE
  ( dce
  , dceExpr
  , dceForeignModule
  ) where

import           Prelude.Compat
import           Control.Arrow ((***))
import           Control.Monad
import           Data.Graph
import           Data.List (any, elem, filter, groupBy, sortBy)
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.JavaScript.Parser.AST
                  ( JSStatement(..)
                  , JSExpression(..)
                  , JSCommaList(..)
                  , JSBlock(..)
                  , JSSwitchParts(..)
                  , JSTryCatch(..)
                  , JSTryFinally(..)
                  , JSArrayElement(..)
                  , JSObjectProperty(..)
                  , JSCommaTrailingList(..)
                  )
import           Language.PureScript.CoreFn
import           Language.PureScript.Names

import Debug.Trace

type Key = Qualified Ident

data DCEVertex a
  = BindVertex (Bind a)
  | ForeignVertex (Qualified Ident)

dce :: forall t a. Show a => [ModuleT t a] -> [Qualified Ident] -> [ModuleT t a]
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

    traceM ("Module: " ++ show moduleName)
    return $ Module moduleComments moduleName imports exports foreigns (dceExpr `map` decls)
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
        onExpr (Var _ i) = [i | isQualified i]
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

getBindIdentsWithExpr :: Bind a -> [(Ident, Expr a)]
getBindIdentsWithExpr (NonRec _ i e) = [(i,e)]
getBindIdentsWithExpr (Rec is) = map (\((_, i),e) -> (i, e)) is

-- DCE of local identifiers
-- detect and remove unused bindings
dceExpr :: forall a. Show a => Bind a -> Bind a
dceExpr = go
  where
  (go, _, _) = everywhereOnValues id exprFn id

  exprFn :: Expr a -> Expr a
  exprFn (Let ann bs ex) =
    let nbs = foldr bindFn [] bs
    in if null nbs
      then ex
      else Let ann nbs ex

    where
    bindFn :: Bind a -> [Bind a] -> [Bind a]
    bindFn b@(NonRec _ i _) r | i `elem` reachableIdents = b : r
                              | otherwise = r
    bindFn (Rec l) r =
      let l' = filter (\((_, i), _) -> i `elem` reachableIdents) l
      in if null l'
          then r
          else Rec l' : r

    (graph, keyForVertex, vertexForKey) = graphFromEdges verts

    -- | Build list of vertices
    --
    -- Under the assumption that all identifiers are unique, which is
    -- fullfiled by PureScript.
    verts :: [(Ident, Ident, [Ident])]
    verts = do
      let bes = getBindIdentsWithExpr `concatMap` bs
      (i, e) <- bes
      let deps = fst `map` filter ((\i' -> i' /= i && isUsedInExpr i' e)  . fst) bes
      return (i, i, deps)

    entryPointVertices :: [Vertex]
    entryPointVertices = catMaybes $ do
      (_, i, _) <- verts
      guard $ isUsedInExpr i ex
      return (vertexForKey i)

    reachableIdents = map ((\(_, i, _) -> i) . keyForVertex) $ reachable graph `concatMap` entryPointVertices
  exprFn e = e

  isUsedInExpr :: Ident -> Expr a -> Bool
  isUsedInExpr i (Literal _ (ArrayLiteral es)) = any (isUsedInExpr i) es
  isUsedInExpr i (Literal _ (ObjectLiteral es)) = any (isUsedInExpr i . snd) es
  isUsedInExpr _ (Literal _ _) = False
  isUsedInExpr i (Constructor _ _ _ is) = i `elem` is
  isUsedInExpr i (Accessor _ _ e) = isUsedInExpr i e
  isUsedInExpr i (ObjectUpdate _ e ups) = isUsedInExpr i e || any (isUsedInExpr i . snd) ups
  isUsedInExpr i (App _ (Abs _ i' e) r)
    = if isUsedInExpr i r
        then isUsedInExpr i' e
        else isUsedInExpr i e
  isUsedInExpr i (App _ l r) = isUsedInExpr i l || isUsedInExpr i r
  isUsedInExpr i (Abs _ i' e) = i /= i' && isUsedInExpr i e
  isUsedInExpr i (Var _ qi) = qi == Qualified Nothing i
  isUsedInExpr i (Case _ es alts) = any (isUsedInExpr i) es || any (isUsedInCaseAlternative i) alts
  isUsedInExpr i (Let _ bs e) =
    if shadowed
        then used
        else used || isUsedInExpr i e
    where
    -- |
    -- Check if an identifier is used in bindings and the resulting
    -- expression.  A binding might shadow an identifier.  The first Boolean
    -- value denotes if i is used in any bind expression, the second if it was
    -- shadowed.
    (used, shadowed) = foldl fn (False, False) (concatMap getBindIdentsWithExpr bs)

    fn (u, s) (i', e')
      | s || i == i'  = (u, True)
      | otherwise     = (u || isUsedInExpr i e', False)

  isUsedInCaseAlternative i (CaseAlternative bs ee) =
    not (any (\b -> case b of
                    VarBinder _ i'  -> i == i'
                    _               -> False) bs
        )
    &&
      (case ee of
        Right e -> isUsedInExpr i e
        Left es -> any (uncurry (||) . (isUsedInExpr i *** isUsedInExpr i)) es)

-- | foldr over `JSCommaList`
foldrJSCommaList :: (a -> b -> b) -> JSCommaList a -> b -> b
foldrJSCommaList _ JSLNil b = b
foldrJSCommaList fn (JSLOne a) b = fn a b
foldrJSCommaList fn (JSLCons as _ a) b = foldrJSCommaList fn as (fn a b)

-- | Filter export statements in a foreign module.  This is not 100% safe.  It
-- might remove declarations that are used somewhere in the foreign module
-- (for example by using `eval`).
dceForeignModule :: [Ident] -> [JSStatement] -> [JSStatement]
dceForeignModule is stmts = filter filterExports stmts

  where
  filterExports :: JSStatement -> Bool
  filterExports (JSAssignStatement (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ x) _) _ _ _)
    = fltr (unquote . T.pack $ x)
  filterExports (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ x)) _ _ _)
    = fltr (T.pack x)
  filterExports _ = True

  fltr :: Text -> Bool
  fltr t = any (fromMaybe True . (path graph <$> vertexForKey t <*>) . Just) entryPointVertices
        -- ^ one of `entryPointVertices` depend on this vertex
        || any (isUsedInStmt t) nonExps
        -- ^ it is used in any non export statements

  -- |
  -- Build a graph of exports statements.  Its initial set of edges point from
  -- an export statement to all other export statements that are using it.
  -- When checking if we need to include that vartex we just check if there is
  -- a path from a vertex to one of `entryPointVertices`.
  exps :: [JSStatement]
  exps = filter isExportStatement stmts

  nonExps = filter (not . isExportStatement) stmts

  (graph, _, vertexForKey) = graphFromEdges verts

  verts :: [(JSStatement, Text, [Text])]
  verts = mapMaybe toVert exps
    where
    toVert :: JSStatement -> Maybe (JSStatement, Text, [Text])
    toVert s
      | Just name <- exportStatementName s = Just (s, name, foldr (fn name) [] exps)
      | otherwise = Nothing

    fn name s' nms
      | isUsedInStmt name s'
      , Just n <- exportStatementName s' = n:nms
      | otherwise = nms

  entryPointVertices :: [Vertex]
  entryPointVertices = catMaybes $ do
    (_, k, _) <- verts
    guard $ k `elem` ns
    return (vertexForKey k)
    where
    ns = runIdent <$> is

  unquote :: Text -> Text
  unquote = T.drop 1 . T.dropEnd 1

  isExportStatement :: JSStatement -> Bool
  isExportStatement (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ _)) _ _ _) = True
  isExportStatement (JSAssignStatement (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ _) _) _ _ _) = True
  isExportStatement _ = False

  exportStatementName :: JSStatement -> Maybe Text
  exportStatementName (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ i)) _ _ _) = Just . T.pack $ i
  exportStatementName (JSAssignStatement (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ i) _) _ _ _) = Just . unquote . T.pack $ i
  exportStatementName _ = Nothing

  -- |
  -- Check if (export) identifier is used within a JSStatement.
  isUsedInStmt :: Text -> JSStatement -> Bool
  isUsedInStmt n (JSStatementBlock _ ss _ _) = any (isUsedInStmt n) ss
  isUsedInStmt n (JSDoWhile _ stm _ _ e _ _) = isUsedInStmt n stm || isUsedInExpr n e
  isUsedInStmt n (JSFor _ _ es1 _ es2 _ es3 _ s) = isUsedInExprs n es1 || isUsedInExprs n es2 || isUsedInExprs n es3 || isUsedInStmt n s
  isUsedInStmt n (JSForIn _ _ e1 _ e2 _ s) = isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
  isUsedInStmt n (JSForVar _ _ _ es1 _ es2 _ es3 _ s) = isUsedInExprs n es1 || isUsedInExprs n es2 || isUsedInExprs n es3 || isUsedInStmt n s
  isUsedInStmt n (JSForVarIn _ _ _ e1 _ e2 _ s) = isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
  isUsedInStmt n (JSFunction _ _ _ _ _ (JSBlock _ ss _) _) = any (isUsedInStmt n) ss
  isUsedInStmt n (JSIf _ _ e _ s) = isUsedInExpr n e || isUsedInStmt n s
  isUsedInStmt n (JSIfElse _ _ e _ s1 _ s2) = isUsedInExpr n e || isUsedInStmt n s1 || isUsedInStmt n s2
  isUsedInStmt n (JSLabelled _ _ s) = isUsedInStmt n s
  isUsedInStmt _ (JSEmptyStatement _) = False
  isUsedInStmt n (JSExpressionStatement e _) = isUsedInExpr n e
  isUsedInStmt n (JSAssignStatement e1 _ e2 _) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInStmt n (JSMethodCall e _ es _ _) = isUsedInExpr n e || isUsedInExprs n es
  isUsedInStmt n (JSReturn _ me _) = fromMaybe False (isUsedInExpr n <$> me)
  isUsedInStmt n (JSSwitch _ _ e _ _ sps _ _) = isUsedInExpr n e || any (isUsedInSwitchParts n) sps
  isUsedInStmt n (JSThrow _ e _) = isUsedInExpr n e
  isUsedInStmt n (JSTry _ (JSBlock _ ss _) cs f) = any (isUsedInStmt n) ss || any (isUsedInTryCatch n) cs || isUsedInFinally n f
  isUsedInStmt n (JSVariable _ es _) = isUsedInExprs n es
  isUsedInStmt n (JSWhile _ _ e _ s) = isUsedInExpr n e || isUsedInStmt n s
  isUsedInStmt n (JSWith _ _ e _ s _) = isUsedInExpr n e || isUsedInStmt n s
  isUsedInStmt _ JSBreak{} = False
  isUsedInStmt _ JSConstant{} = False
  isUsedInStmt _ JSContinue{} = False

  -- |
  -- Check is (export) identifier is used withing a JSExpression
  isUsedInExpr :: Text -> JSExpression -> Bool
  isUsedInExpr n (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ i)) = n == T.pack i
  isUsedInExpr n (JSMemberDot e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInExpr n (JSArrayLiteral _ as _) = any (isUsedInArrayElement n) as
  isUsedInExpr n (JSAssignExpression e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInExpr n (JSCallExpression e _ es _) = isUsedInExpr n e || isUsedInExprs n es
  isUsedInExpr n (JSCallExpressionDot e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInExpr n (JSCallExpressionSquare e1 _ e2 _) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInExpr n (JSExpressionBinary e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInExpr n (JSExpressionParen _ e _) = isUsedInExpr n e
  isUsedInExpr n (JSExpressionPostfix e _) = isUsedInExpr n e
  isUsedInExpr n (JSExpressionTernary e1 _ e2 _ e3) = isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInExpr n e3
  isUsedInExpr n (JSFunctionExpression _ _ _ _ _ (JSBlock _ ss _)) = any (isUsedInStmt n) ss
  isUsedInExpr n (JSMemberExpression e _ es _) = isUsedInExpr n e || isUsedInExprs n es
  isUsedInExpr n (JSMemberNew _ e _ es _) = isUsedInExpr n e || isUsedInExprs n es
  isUsedInExpr n (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ i) _) = n == (unquote .T.pack $ i)
  isUsedInExpr n (JSMemberSquare e1 _ e2 _) = isUsedInExpr n e1 || isUsedInExpr n e2
  isUsedInExpr n (JSNewExpression _ e) = isUsedInExpr n e
  isUsedInExpr n (JSObjectLiteral _ ops _) = foldrJSCommaList (\p b -> isUsedInObjectProperty n p || b) (fromCTList ops) False
    where
    fromCTList (JSCTLComma as _) = as
    fromCTList (JSCTLNone as) = as
  isUsedInExpr n (JSUnaryExpression _ e) = isUsedInExpr n e
  isUsedInExpr n (JSVarInitExpression e _) = isUsedInExpr n e
  isUsedInExpr _ JSIdentifier{} = False
  isUsedInExpr _ JSDecimal{} = False
  isUsedInExpr _ JSLiteral{} = False
  isUsedInExpr _ JSHexInteger{} = False
  isUsedInExpr _ JSOctal{} = False
  isUsedInExpr _ JSStringLiteral{} = False
  isUsedInExpr _ JSRegEx{} = False
  isUsedInExpr n (JSCommaExpression e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2

  isUsedInExprs :: Text -> JSCommaList JSExpression -> Bool
  isUsedInExprs n es = foldrJSCommaList fn es False
    where
    fn :: JSExpression -> Bool -> Bool
    fn e b = isUsedInExpr n e || b

  -- |
  -- Check if (export) identifier is used withing a JSSitchParts
  isUsedInSwitchParts :: Text -> JSSwitchParts -> Bool
  isUsedInSwitchParts n (JSCase _ e _ ss) = isUsedInExpr n e || any (isUsedInStmt n) ss
  isUsedInSwitchParts n (JSDefault _ _ ss) = any (isUsedInStmt n) ss

  -- |
  -- Check if (export) identifier is used withing a JSTryCatch
  isUsedInTryCatch :: Text -> JSTryCatch -> Bool
  isUsedInTryCatch n (JSCatch _ _ e _ (JSBlock _ ss _)) = isUsedInExpr n e || any (isUsedInStmt n) ss
  isUsedInTryCatch n (JSCatchIf _ _ e1 _ e2 _ (JSBlock _ ss _)) = isUsedInExpr n e1 || isUsedInExpr n e2 || any (isUsedInStmt n) ss

  -- |
  -- Check if (export) identifier is used withing a JSTryFinally
  isUsedInFinally :: Text -> JSTryFinally -> Bool
  isUsedInFinally n (JSFinally _ (JSBlock _ ss _)) = any (isUsedInStmt n) ss
  isUsedInFinally _ JSNoFinally = False

  -- |
  -- Check if (export) identifier is used withing a JSArrayElement
  isUsedInArrayElement :: Text -> JSArrayElement -> Bool
  isUsedInArrayElement n (JSArrayElement e) = isUsedInExpr n e
  isUsedInArrayElement _ JSArrayComma{} = False

  -- |
  -- Check if (export) identifier is used withing a JSObjectProperty
  isUsedInObjectProperty :: Text -> JSObjectProperty -> Bool
  isUsedInObjectProperty n (JSPropertyAccessor _ _ _ es _ (JSBlock _ ss _)) = any (isUsedInExpr n) es || any (isUsedInStmt n) ss
  isUsedInObjectProperty n (JSPropertyNameandValue _ _ es) = any (isUsedInExpr n) es
