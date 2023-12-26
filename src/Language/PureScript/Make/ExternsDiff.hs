module Language.PureScript.Make.ExternsDiff
  ( ExternsDiff
  , emptyDiff
  , diffExterns
  , checkDiffs
  ) where

import Protolude hiding (check, moduleName, trace)

import Data.Graph as G (graphFromEdges, reachable)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations.ChainId (ChainId (..))
import Language.PureScript.Constants.Prim (primModules)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names (ModuleName)
import Language.PureScript.Names qualified as P
import Language.PureScript.Types qualified as P

type RefStatus = Bool

data ExternsDiff = ExternsDiff
  {edModuleName :: ModuleName, edRefs :: M.Map Ref RefStatus}
  deriving (Show)

-- | Empty diff means no effective difference between externs.
emptyDiff :: P.ModuleName -> ExternsDiff
emptyDiff mn = ExternsDiff mn mempty

isRefRemoved :: RefStatus -> Bool
isRefRemoved = not

-- Refs structure appropriate for storing and checking externs diffs.
data Ref
  = TypeClassRef (P.ProperName 'P.ClassName)
  | TypeOpRef (P.OpName 'P.TypeOpName)
  | TypeRef (P.ProperName 'P.TypeName)
  | -- we use separate ref for a data constructor and keep here origin type as well
    ConstructorRef (P.ProperName 'P.TypeName) (P.ProperName 'P.ConstructorName)
  | ValueRef P.Ident
  | ValueOpRef (P.OpName 'P.ValueOpName)
  | -- instance ref points to the class and types defined in the same module
    -- TypeInstanceRef P.Ident (Maybe (P.ProperName 'P.ClassName)) [P.ProperName 'P.TypeName]
    TypeInstanceRef P.Ident (ModuleName, P.ProperName 'P.ClassName) [P.ProperName 'P.TypeName]
  deriving (Show, Eq, Ord)

diffExterns :: P.ExternsFile -> P.ExternsFile -> [ExternsDiff] -> ExternsDiff
diffExterns newExts oldExts depsDiffs =
  ExternsDiff modName $
    addStatus (changedRefs <> affectedReExported <> allAffectedLocalRefs)
  where
    modName = P.efModuleName newExts
    -- Marks if ref was removed
    addStatus = M.fromSet (flip S.notMember removedSet)

    depsDiffsMap = M.fromList (map (liftM2 (,) edModuleName (M.keysSet . edRefs)) depsDiffs)

    -- To get changed reexported refs, we take those which were removed (not
    -- present in new extern's exports) or changed in dependencies.
    goRe (P.ReExportRef _ es ref) = (P.exportSourceDefinedIn es,) <$> toRefs ref
    goRe _ = []

    oldExports = concatMap goRe (P.efExports oldExts)
    newReExports = concatMap goRe (P.efExports newExts)
    checkRe (mn, ref)
      | (mn, ref) `notElem` newReExports = True
      | Just True <- elem ref <$> M.lookup mn depsDiffsMap = True
      | otherwise = False
    affectedReExported = S.fromList $ map snd $ filter checkRe oldExports

    getDecls = map stripDeclaration . P.efDeclarations
    getTypeFixities = P.efTypeFixities
    getFixities = P.efFixities

    -- Type class instances if changed (added/removed) indirectly effect back
    -- the class or the types that are defined in the module, meaning if the
    -- instance is added/removed we will recompile modules that use the type
    -- class or (if the type class defined in another module) we have to
    -- recompile modules that use types defined in this module affected by the
    -- instance.
    applyInstances (a, r, c, u) =
      let checkType t (TypeRef t') = t' == t
          checkType _ _ = False
          uRefs = map fst u
          go (TypeInstanceRef _ (clsMod, cls) types)
            | clsRef <- TypeClassRef cls =
                if clsMod == modName
                  then -- If the class is defined in this module we ensure that is marked as changed
                    maybe [] pure $ find ((==) clsRef) uRefs
                  else case S.member clsRef <$> M.lookup clsMod depsDiffsMap of
                    Just True ->
                      -- if the type class is in another module and it has
                      -- changed we don't need to care about instance types.
                      []
                    -- Otherwise mark instance types as changed.
                    _ ->
                      foldMap (\t -> filter (checkType t) uRefs) types
          go _ = mempty
          affected = foldMap (S.fromList . go . fst) (a <> r <> c)
          (uc, uu) = L.partition (flip S.member affected . fst) u
       in (a, r, c <> uc, uu)

    declsSplit =
      applyInstances $
        splitRefs (getDecls newExts) (getDecls oldExts) (externsDeclarationToRef modName)

    -- Make the context for fixity's data constructor search: place all
    -- known refs in the map.
    getRefsSet (a, r, c, u) = S.fromList $ map fst (a <> r <> c <> u)
    fixityCtx = M.insert modName (getRefsSet declsSplit) depsDiffsMap

    -- Determine which  declarations  where directly changed or removed.
    (_, removed, changed, unchangedRefs) =
      foldl
        zipTuple4
        (mempty, mempty, mempty, mempty)
        [ declsSplit
        , splitRefs (getFixities newExts) (getFixities oldExts) (pure . externsFixityToRef fixityCtx)
        , splitRefs (getTypeFixities newExts) (getTypeFixities oldExts) (pure . externsTypeFixityToRef)
        ]

    removedSet = S.fromList (map fst removed)
    changedRefs = S.fromList $ map fst (removed <> changed)

    diffsMapWithLocal
      | null changedRefs = depsDiffsMap
      | otherwise = M.insert modName changedRefs depsDiffsMap

    -- Affected refs here are refs that depend on external or local changed refs.
    --
    -- Rest local refs are refs that do not depend on external/local changed, but
    -- may depend on affected local refs and need to be checked.
    hasChangedDeps (mn, ref) =
      Just True == (S.member ref <$> M.lookup mn diffsMapWithLocal)
    (affectedLocalRefs, restLocalRefs) =
      L.partition (any hasChangedDeps . snd) unchangedRefs

    -- Use graph to go though local refs and their cyclic dependencies on each other.
    -- The graph includes only local refs that depend on other local refs.
    toNode (ref, deps) = (ref, ref, map snd $ filter ((== modName) . fst) deps)

    vtxs = toNode <$> (map (map S.toList) restLocalRefs <> (map (const mempty) <$> affectedLocalRefs))
    (graph, fromVtx, toVtx) = G.graphFromEdges vtxs
    refsGraph = do
      (_, t, _) <- vtxs
      let v = fromMaybe (internalError "diffExterns: vertex not found") $ toVtx t
      let deps = G.reachable graph v
      let toKey = (\(_, k, _) -> k) . fromVtx
      pure (t, map toKey deps)

    -- Get local refs that depend on affected refs (affected refs are included
    -- in the graph too).
    allAffectedLocalRefs =
      S.fromList $
        map fst $
          filter (any (flip elem (fst <$> affectedLocalRefs)) . snd) refsGraph

checkDiffs :: P.Module -> [ExternsDiff] -> Bool
checkDiffs (P.Module _ _ _ decls exports) diffs
  | all isEmpty diffs = False
  | isNothing mbSearch = True
  | null searches = False
  | otherwise = checkReExports || checkUsage searches decls
  where
    mbSearch = makeSearches decls diffs
    searches = fromMaybe S.empty mbSearch
    -- Check if the module reexports any of searched refs.
    checkReExports = flip (maybe False) exports $ any $ \case
      P.ModuleRef _ mn -> not . null $ S.filter ((== Just mn) . fst) searches
      _ -> False

-- Goes though the module and try to find any usage of the refs.
checkUsage :: Set (Maybe ModuleName, Ref) -> [P.Declaration] -> Bool
checkUsage searches decls = foldMap findUsage decls /= mempty
  where
    findUsage decl =
      let (extr, _, _, _, _) = P.everythingWithScope goDecl goExpr goBinder mempty mempty
       in extr mempty decl

    toSearched = (,) <$> P.getQual <*> P.disqualify

    -- To check data constructors we remove an origin type from it.
    emptyName = P.ProperName ""
    stripCtorType (ConstructorRef _ n) = ConstructorRef emptyName n
    stripCtorType x = x

    searches' = S.map (map stripCtorType) searches
    check = (\x -> [x | x]) . flip S.member searches' . toSearched

    checkType = check . map TypeRef
    checkTypeOp = check . map TypeOpRef
    checkValue = check . map ValueRef
    checkValueOp = check . map ValueOpRef
    checkCtor = check . map (ConstructorRef emptyName)
    checkClass = check . map TypeClassRef

    onTypes = P.everythingOnTypes (<>) $ \case
      P.TypeConstructor _ n -> checkType n
      P.TypeOp _ n -> checkTypeOp n
      P.ConstrainedType _ c _ -> checkClass (P.constraintClass c)
      _ -> mempty

    foldCtor f (P.DataConstructorDeclaration _ _ vars) =
      foldMap (f . snd) vars

    constraintTypes =
      foldMap (\c -> P.constraintArgs c <> P.constraintKindArgs c)

    goDecl _ = \case
      P.TypeDeclaration t -> onTypes (P.tydeclType t)
      P.DataDeclaration _ _ _ _ ctors -> foldMap (foldCtor onTypes) ctors
      P.TypeSynonymDeclaration _ _ _ t -> onTypes t
      P.KindDeclaration _ _ _ t -> onTypes t
      P.FixityDeclaration _ (Right (P.TypeFixity _ tn _)) ->
        checkType tn
      P.FixityDeclaration _ (Left (P.ValueFixity _ (P.Qualified by val) _)) ->
        either (checkValue . P.Qualified by) (checkCtor . P.Qualified by) val
      P.TypeClassDeclaration _ _ _ cs _ _ ->
        foldMap onTypes (constraintTypes cs)
      P.TypeInstanceDeclaration _ _ _ _ _ cs tc sts _ ->
        foldMap onTypes (constraintTypes cs <> sts) <> checkClass tc
      _ -> mempty

    isLocal scope ident = P.LocalIdent ident `S.member` scope
    goExpr scope expr = case expr of
      P.Var _ n
        | P.isUnqualified n && isLocal scope (P.disqualify n) -> mempty
        | otherwise -> checkValue n
      P.Constructor _ n -> checkCtor n
      P.Op _ n -> checkValueOp n
      P.TypedValue _ _ t -> onTypes t
      _ -> mempty

    goBinder _ binder = case binder of
      P.ConstructorBinder _ n _ -> checkCtor n
      P.OpBinder _ n -> checkValueOp n
      _ -> mempty

-- | Traverses imports and returns a set of refs to be searched though the
-- module. Returns Nothing if removed refs found in imports (no need to search
-- through the module). If an empty set is returned then no changes apply to the
-- module.
makeSearches :: [P.Declaration] -> [ExternsDiff] -> Maybe (Set (Maybe ModuleName, Ref))
makeSearches decls depsDiffs =
  foldM go mempty decls
  where
    diffsMap = M.fromList (map (liftM2 (,) edModuleName edRefs) depsDiffs)

    -- Add data constructors to refs if all are implicitly imported using (..).
    getCtor n (ConstructorRef tn _) = tn == n
    getCtor _ _ = False
    getCtors n = M.keys . M.filterWithKey (const . getCtor n)
    addCtors mn (P.TypeRef _ n Nothing) = maybe [] (getCtors n) (M.lookup mn diffsMap)
    addCtors _ _ = []
    getRefs = (toRefs <>) . addCtors

    go s (P.ImportDeclaration _ mn dt qual)
      -- We return Nothing if we encounter removed refs in imports.
      | Just diffs <- M.lookup mn diffsMap
      , removed <- M.keysSet $ M.filter isRefRemoved diffs =
          fmap ((s <>) . S.map (qual,) . M.keysSet) $ case dt of
            P.Explicit dRefs
              | any (flip S.member removed) refs -> Nothing
              | otherwise ->
                  -- search only refs encountered in the import.
                  Just $ M.filterWithKey (const . flip elem refs) diffs
              where
                refs = foldMap (getRefs mn) dRefs
            P.Hiding dRefs
              | any (flip S.member removed) refs -> Nothing
              | otherwise ->
                  -- search only refs not encountered in the import.
                  Just $ M.filterWithKey (const . not . flip elem refs) diffs
              where
                refs = foldMap (getRefs mn) dRefs
            -- search all changed refs
            P.Implicit -> Just diffs
    go s _ = Just s

toRefs :: P.DeclarationRef -> [Ref]
toRefs = \case
  P.TypeClassRef _ n -> [TypeClassRef n]
  P.TypeOpRef _ n -> [TypeOpRef n]
  P.TypeRef _ n c -> [TypeRef n] <> (ConstructorRef n <$> fromMaybe [] c)
  P.ValueRef _ i -> [ValueRef i]
  P.ValueOpRef _ n -> [ValueOpRef n]
  _ -> []

isEmpty :: ExternsDiff -> Bool
isEmpty (ExternsDiff _ refs)
  | null refs = True
  | otherwise = False

type Tuple4 m a = (m a, m a, m a, m a)

zipTuple4 :: Monoid (m a) => Tuple4 m a -> Tuple4 m a -> Tuple4 m a
zipTuple4 (f1, s1, t1, fo1) (f2, s2, t2, fo2) =
  (f1 <> f2, s1 <> s2, t1 <> t2, fo1 <> fo2)

-- | Returns refs as a tuple of four (added, removed, changed, unchanged).
splitRefs :: Ord r => Eq a => [a] -> [a] -> (a -> Maybe r) -> Tuple4 [] r
splitRefs new old toRef =
  M.foldrWithKey go (added, [], [], []) oldMap
  where
    toMap = M.fromList . mapMaybe (((<$>) . flip (,)) <*> toRef)
    newMap = toMap new
    oldMap = toMap old
    added = M.keys $ M.difference newMap oldMap
    go ref decl (a, r, c, u) = case M.lookup ref newMap of
      Nothing -> (a, r <> [ref], c, u)
      Just newDecl
        | decl /= newDecl -> (a, r, c <> [ref], u)
        | otherwise -> (a, r, c, u <> [ref])

-- | Traverses the type and finds all the refs within.
typeDeps :: P.Type a -> S.Set (ModuleName, Ref)
typeDeps = P.everythingOnTypes (<>) $
  \case
    P.TypeConstructor _ (P.Qualified (P.ByModuleName mn) tn)
      | isPrimModule mn -> mempty
      | otherwise -> S.singleton (mn, TypeRef tn)
    P.TypeConstructor _ _ ->
      internalError "typeDeps: type is not qualified"
    P.TypeOp _ (P.Qualified (P.ByModuleName mn) tn)
      | isPrimModule mn -> mempty
      | otherwise -> S.singleton (mn, TypeOpRef tn)
    P.ConstrainedType _ c _ ->
      S.singleton (map TypeClassRef (qualified $ P.constraintClass c))
    P.TypeOp _ _ ->
      internalError "typeDeps: type is not qualified"
    _ -> mempty

qualified :: P.Qualified b -> (ModuleName, b)
qualified (P.Qualified (P.ByModuleName mn) v) = (mn, v)
qualified _ = internalError "ExternsDiff: type is not qualified"

type RefWithDeps = (Ref, S.Set (ModuleName, Ref))

-- | To get fixity's data constructor dependency we should provide it with the
-- context (that contains all known refs) to search in.
externsFixityToRef :: Map ModuleName (Set Ref) -> P.ExternsFixity -> RefWithDeps
externsFixityToRef refs (P.ExternsFixity _ _ n alias) =
  (ValueOpRef n, maybe mempty S.singleton $ getDep (qualified alias))
  where
    getDep (mn, Left i) = Just (mn, ValueRef i)
    getDep (mn, Right p) =
      (mn,) <$> (M.lookup mn refs >>= S.lookupMin . S.filter (goRef p))
    goRef c (ConstructorRef _ c') = c' == c
    goRef _ _ = False

externsTypeFixityToRef :: P.ExternsTypeFixity -> RefWithDeps
externsTypeFixityToRef (P.ExternsTypeFixity _ _ n alias) =
  ( TypeOpRef n
  , S.singleton (map TypeRef (qualified alias))
  )

externsDeclarationToRef :: ModuleName -> P.ExternsDeclaration -> Maybe RefWithDeps
externsDeclarationToRef moduleName = \case
  P.EDType n t tk
    | isDictName n -> Nothing
    | otherwise -> Just (TypeRef n, typeDeps t <> typeKindDeps tk)
  --
  P.EDTypeSynonym n args t ->
    Just (TypeRef n, typeDeps t <> foldArgs args)
  --
  P.EDDataConstructor n _ tn t _
    | isDictName n -> Nothing
    | otherwise ->
        Just
          ( ConstructorRef tn n
          , -- Add the type as a dependency: if the type has changed (e.g.
            -- constructors removed/added) it should affect all the constructors
            -- in the type.
            S.insert (moduleName, TypeRef tn) (typeDeps t)
          )
  --
  P.EDValue n t ->
    Just (ValueRef n, typeDeps t)
  --
  P.EDClass n args members constraints _ _ ->
    Just
      ( TypeClassRef n
      , foldArgs args <> constraintsDeps constraints <> foldMap (typeDeps . snd) members
      )
  --
  P.EDInstance cn n args kinds types constraints _ _ _ _ ->
    Just
      ( TypeInstanceRef n (qualified cn) (mapMaybe myType types)
      , maybe mempty constraintsDeps constraints <> instanceArgsDeps args <> foldMap typeDeps kinds
      )
  where
    goDataTypeArg (_, st, _) = maybe mempty typeDeps st
    typeKindDeps (P.DataType _ args _) = foldMap goDataTypeArg args
    typeKindDeps _ = mempty

    myType (P.TypeConstructor _ (P.Qualified (P.ByModuleName mn) tn))
      | isPrimModule mn || moduleName /= mn = Nothing
      | otherwise = Just tn
    myType _ = Nothing

    foldArgs = foldMap typeDeps . mapMaybe snd
    instanceArgsDeps = foldMap (typeDeps . snd)
    constraintsDeps =
      foldMap
        ( \(P.Constraint _ cls kArgs args _) ->
            S.singleton (TypeClassRef <$> qualified cls)
              <> foldMap typeDeps kArgs
              <> foldMap typeDeps args
        )

-- | Removes excessive info from declarations before comparing.
stripDeclaration :: P.ExternsDeclaration -> P.ExternsDeclaration
stripDeclaration = \case
  P.EDType n t (P.DataType dt args ctors) ->
    -- Remove data constructors types, we don't need them, we only need to know
    -- if the list of ctors has changed.
    P.EDType n t (P.DataType dt args (map (map (const [])) ctors))
  --
  P.EDInstance cn n fa ks ts cs ch chi ns ss ->
    P.EDInstance cn n fa ks ts cs (map stripChain ch) chi ns ss
  --
  decl -> decl
  where
    emptySP = P.SourcePos 0 0
    -- emptySS = SourceSpan "" emptySP emptySP
    stripChain (ChainId (n, _)) = ChainId (n, emptySP)

isPrimModule :: ModuleName -> Bool
isPrimModule = flip S.member (S.fromList primModules)

-- | Check if type name is a type class dictionary name.
isDictName :: P.ProperName a -> Bool
isDictName =
  T.isInfixOf "$" . P.runProperName
