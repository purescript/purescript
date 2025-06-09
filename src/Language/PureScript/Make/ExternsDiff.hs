module Language.PureScript.Make.ExternsDiff
  ( ExternsDiff(..)
  , RefStatus(..)
  , DiffRef(..)
  , Ref(..)
  , isEmpty
  , emptyDiff
  , diffExterns
  , checkDiffs
  ) where

import Protolude hiding (check, moduleName, trace)

import Data.Graph as G (graphFromEdges, reachable)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations.ChainId (ChainId (..))
import Language.PureScript.Constants.Prim (primModules)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names (ModuleName)
import Language.PureScript.Names qualified as P
import Language.PureScript.Types qualified as P

-- Refs structure appropriate for storing and checking externs diffs.
data Ref
  = TypeClassRef (P.ProperName 'P.ClassName)
  | TypeOpRef (P.OpName 'P.TypeOpName)
  | TypeRef (P.ProperName 'P.TypeName)
  | -- We use separate ref for a data constructor and keep here origin type as well.
    ConstructorRef (P.ProperName 'P.TypeName) (P.ProperName 'P.ConstructorName)
  | -- A ad-hoc ref that points to the type with a set of constructors that changed.
    -- It is needed to correctly handle effects of adding/removing of ctors.
    CtorsSetRef (P.ProperName 'P.TypeName)
  | ValueRef P.Ident
  | ValueOpRef (P.OpName 'P.ValueOpName)
  | -- Instance ref points to the class and types defined in the same module.
    TypeInstanceRef P.Ident (ModuleName, P.ProperName 'P.ClassName) [P.ProperName 'P.TypeName]
  deriving (Eq, Ord, Show)

-- In diff we track removed, changed) and added refs.
data RefStatus = Added | Removed | Updated
  deriving (Eq, Ord, Show)

type RefWithDeps = (Ref, S.Set (ModuleName, Ref))

type RefsWithStatus = M.Map Ref RefStatus

type ModuleRefsMap = Map ModuleName (Set Ref)

data ExternsDiff = ExternsDiff
  { edModuleName :: ModuleName, edRefs :: Map Ref RefStatus }
  deriving (Eq, Ord, Show)

-- | Empty diff means no effective difference between externs.
emptyDiff :: P.ModuleName -> ExternsDiff
emptyDiff mn = ExternsDiff mn mempty

isRefRemoved :: RefStatus -> Bool
isRefRemoved Removed = True
isRefRemoved _ = False

-- To get changed reexported refs, we take those which were removed (not
-- present in new extern's exports) or changed in dependencies.
getReExported :: P.ExternsFile -> P.ExternsFile -> ModuleRefsMap -> RefsWithStatus
getReExported newExts oldExts depsDiffsMap =
  M.fromList $ mapMaybe checkRe oldExports
  where
    goRe (P.ReExportRef _ es ref) = (P.exportSourceDefinedIn es,) <$> toRefs ref
    goRe _ = []

    oldExports = concatMap goRe (P.efExports oldExts)
    newReExports = concatMap goRe (P.efExports newExts)
    checkRe (mn, ref)
      | (mn, ref) `notElem` newReExports = Just (ref, Removed)
      | Just True <- elem ref <$> M.lookup mn depsDiffsMap = Just (ref, Updated)
      | otherwise = Nothing

-- Extracts declarations from old and new externs and compares them. Returns a
-- tuple of changed refs (a form of which have changed) and unchanged refs with
-- dependencies (refs they depend upon).
getChanged :: P.ExternsFile -> P.ExternsFile -> ModuleRefsMap -> (RefsWithStatus, [RefWithDeps])
getChanged newExts oldExts depsDiffsMap =
  (changedRefs, unchangedRefs)
  where
    modName = P.efModuleName newExts

    getDecls = map refineDeclaration . P.efDeclarations
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
          uRefs = map fst u -- Unchanged refs.
          go (TypeInstanceRef _ (clsMod, cls) types)
            | clsRef <- TypeClassRef cls =
                if clsMod == modName
                  then -- If the class is defined in this module we ensure that is marked as changed.
                    maybe [] pure $ find ((==) clsRef) uRefs
                  else case S.member clsRef <$> M.lookup clsMod depsDiffsMap of
                    Just True ->
                      -- If the type class is in another module and it has
                      -- changed we don't need to care about instance types
                      -- (because the instance change affects modules that use
                      -- the type class/its methods).
                      []
                    _ ->
                      -- Otherwise mark instance types as changed.
                      foldMap (\t -> filter (checkType t) uRefs) types
          go _ = mempty

          -- Check class instances in added, removed and changed.
          affected = foldMap (S.fromList . go . fst) (a <> r <> c)
          (uc, uu) = L.partition (flip S.member affected . fst) u
       in (a, r, c <> uc, uu)

    -- Group/split exported refs of the module into (added, removed, changed,
    -- unchanged) - (a, r, c, u).
    declsSplit =
      applyInstances $
        splitRefs (getDecls newExts) (getDecls oldExts) (externsDeclarationToRef modName)

    -- Make the context for fixity's data constructor search: place all
    -- known refs in the map.
    getRefsSet (a, r, c, u) = S.fromList $ map fst (a <> r <> c <> u)
    fixityCtx = M.insert modName (getRefsSet declsSplit) depsDiffsMap

    -- Determine which declarations where directly changed or removed by
    -- combining Declarations, Fixities and Type Fixities - as they are
    -- separated in externs we handle them separately. We don't care about added things.
    (added, removed, changed, unchangedRefs) =
      fold
        [ declsSplit
        , splitRefs (getFixities newExts) (getFixities oldExts) (pure . externsFixityToRef fixityCtx)
        , splitRefs (getTypeFixities newExts) (getTypeFixities oldExts) (pure . externsTypeFixityToRef)
        ]

    withStatus status refs =  map ((,status) . fst) refs
    changedRefs =
      M.fromList $
        withStatus Added added <> withStatus Removed removed <> withStatus Updated changed

-- Gets set of type constructors from new externs that have changed.
getCtorsSets :: P.ExternsFile -> P.ExternsFile -> Set Ref
getCtorsSets newExts oldExts =
  S.map CtorsSetRef $
    M.keysSet $
      M.differenceWith comp (getSets newExts) (getSets oldExts)
  where
    getSets = M.fromList . foldMap goDecl . P.efDeclarations
    goDecl = \case
      P.EDType n _ (P.DataType _ _ ctors) ->
        [(n, S.fromList $ fst <$> ctors)]
      _ -> []
    comp a b = if a == b then Nothing else Just a

-- Takes a list unchanged local refs with dependencies and finds that are affected by
-- changed refs. Cyclic dependencies between local refs are searched using
-- directed graph.
getAffectedLocal :: ModuleName -> ModuleRefsMap -> [RefWithDeps] -> Set Ref
getAffectedLocal modName diffsMap unchangedRefs =
  affectedLocalRefs
  where
    hasChangedDeps (mn, ref) =
      Just True == (S.member ref <$> M.lookup mn diffsMap)
    (affectedByChanged, restLocalRefs) =
      L.partition (any hasChangedDeps . snd) unchangedRefs

    -- Use graph to go though local refs and their cyclic dependencies on each other.
    -- The graph includes only local refs that depend on other local refs.
    toNode (ref, deps) = (ref, ref, map snd $ filter ((== modName) . fst) (S.toList deps))

    -- Make graph vertexes from the rest local refs with deps and affected refs
    -- with no deps.
    vtxs = toNode <$> restLocalRefs <> (map (const mempty) <$> affectedByChanged)
    (graph, fromVtx, toVtx) = G.graphFromEdges vtxs

    -- Graph is a list of refs with (refs) dependencies.
    refsGraph = do
      (_, t, _) <- vtxs
      let v = fromMaybe (internalError "diffExterns: vertex not found") $ toVtx t
      let deps = G.reachable graph v
      let toKey = (\(_, k, _) -> k) . fromVtx
      pure (t, map toKey deps)

    -- Get local refs that depend on affected refs (affected refs are included
    -- in the graph result because a node's reachable list includes the node
    -- itself).
    affectedLocalRefs =
      S.fromList $
        map fst $
          filter (any (flip elem (fst <$> affectedByChanged)) . snd) refsGraph

-- Compares two externs file versions using list with diffs of dependencies.
diffExterns :: [ExternsDiff] -> P.ExternsFile -> P.ExternsFile -> ExternsDiff
diffExterns depsDiffs newExts oldExts  =
  ExternsDiff modName $
    affectedReExported <> changedRefs <> affectedLocalRefs
  where
    modName = P.efModuleName newExts

    depsDiffsMap = M.fromList (map (liftM2 (,) edModuleName (M.keysSet . edRefs)) depsDiffs)

    -- To get changed reexported refs, we take those which were removed (not
    -- present in new extern's exports) or changed in dependencies.
    affectedReExported = getReExported newExts oldExts depsDiffsMap

    (changedRefs, unchangedRefs) = getChanged newExts oldExts depsDiffsMap

    ctorsSets = getCtorsSets newExts oldExts

    -- Extend dependencies' diffs map with local changes.
    diffsMapWithLocal
      | null changedRefs && null ctorsSets = depsDiffsMap
      | otherwise = M.insert modName (M.keysSet changedRefs <> ctorsSets) depsDiffsMap

    affectedLocalRefs =
      M.fromSet (const Updated) $ getAffectedLocal modName diffsMapWithLocal unchangedRefs

-- This type defines a reason for module to be rebuilt. It contains the fhe
-- first found reference to changed elements.
data DiffRef
    = ImportedRef (ModuleName, Ref)
    | ReExportedRef (ModuleName, Ref)
    | UsedRef (ModuleName, Ref)
    deriving (Show, Eq, Ord)

-- Checks if the module effectively uses any of diff's refs.
checkDiffs :: P.Module -> [ExternsDiff] -> Maybe DiffRef
checkDiffs (P.Module _ _ _ decls exports) diffs
  | all isEmpty diffs = Nothing
  | otherwise = case makeSearches decls diffs of
      Left r -> Just (ImportedRef r)
      Right searches
        | null searches -> Nothing
        | otherwise ->
            (ReExportedRef <$> checkReExports searches)
              <|> (UsedRef <$> checkUsage searches decls)
  where
    -- Check if the module reexports any of searched refs.
    checkReExports searches =
      map (\(mn, _, ref) -> (mn, ref)) $
        exports >>=
         listToMaybe . foldMap
          ( \case
              P.ModuleRef _ mn -> maybeToList $ find (\(_, mn' ,_) -> mn' == Just mn ) searches
              _ -> []
          )

-- Takes a set of refs to search in module's declarations,.
-- Goes though the module and searches for the first usage of any.
checkUsage :: Set (ModuleName, Maybe ModuleName, Ref) -> [P.Declaration] -> Maybe (ModuleName, Ref)
checkUsage searches decls = listToMaybe anyUsages
  where
    -- Two traversals: one to pick up usages of types, one for the rest.
    anyUsages =
      foldMap checkUsageInTypes decls
        <> foldMap checkOtherUsages decls

    -- To check data constructors we remove an origin type from it (see `checkCtor`).
    searches' = S.map (map stripCtorType) searches

    -- To check data constructors we remove an origin type from it.
    emptyName = P.ProperName ""
    stripCtorType (ConstructorRef _ n) = ConstructorRef emptyName n
    stripCtorType x = x

    -- Check if a declaration is searchable element and map it to result.
    check q = maybeToList $ (\(mn, _, ref) -> (mn, ref)) <$>
        find (\(_, qual, ref) -> (qual, ref) == (P.getQual q, P.disqualify q)) searches'

    checkType = check . map TypeRef
    checkTypeOp = check . map TypeOpRef
    checkValue = check . map ValueRef
    checkValueOp = check . map ValueOpRef
    checkCtor = check . map (ConstructorRef emptyName)
    checkClass = check . map TypeClassRef

    -- A nested traversal: pick up types in the module then traverse the structure of the types
    (checkUsageInTypes, _, _, _, _) =
      P.accumTypes $ P.everythingOnTypes (<>) $ \case
        P.TypeConstructor _ n -> checkType n
        P.TypeOp _ n -> checkTypeOp n
        P.ConstrainedType _ c _ -> checkClass (P.constraintClass c)
        _ -> mempty

    checkOtherUsages =
      let (extr, _, _, _, _) = P.everythingWithScope goDecl goExpr goBinder mempty mempty
       in extr mempty

    goDecl _ = \case
      P.TypeInstanceDeclaration _ _ _ _ _ _ tc _ _ ->
        checkClass tc
      _ -> mempty

    isLocal scope ident = P.LocalIdent ident `S.member` scope
    goExpr scope expr = case expr of
      P.Var _ n
        | P.isUnqualified n && isLocal scope (P.disqualify n) -> mempty
        | otherwise -> checkValue n
      P.Constructor _ n -> checkCtor n
      P.Op _ n -> checkValueOp n
      _ -> mempty

    goBinder _ binder = case binder of
      P.ConstructorBinder _ n _ -> checkCtor n
      P.OpBinder _ n -> checkValueOp n
      _ -> mempty


-- | Traverses imports and returns a set of refs to be searched though the
-- module. Returns Left with the first removed ref found in imports (no need to search
-- through the module - the module needs to be recompiled). If an empty set is
-- returned then no changes apply to the module.
makeSearches :: [P.Declaration] -> [ExternsDiff] -> Either (ModuleName, Ref) (Set (ModuleName, Maybe ModuleName, Ref))
makeSearches decls depsDiffs =
  foldM go mempty decls
  where
    diffsMap = M.fromList (map (liftM2 (,) edModuleName edRefs) depsDiffs)
    searchRef = find . flip S.member
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
          fmap ((s <>) . S.map (mn, qual,) . M.keysSet) $ case dt of
            P.Explicit dRefs
              | Just ref <- searchRef removed refs -> Left (mn, ref)
              | otherwise ->
                  -- Search only refs encountered in the import.
                  Right $ M.filterWithKey (const . flip elem refs) diffs
              where
                refs = foldMap (getRefs mn) dRefs
            P.Hiding dRefs
              | Just ref <- searchRef removed refs -> Left (mn, ref)
              | otherwise ->
                  -- Search only refs not encountered in the import.
                  Right $ M.filterWithKey (const . not . flip elem refs) diffs
              where
                refs = foldMap (getRefs mn) dRefs
            -- Search all changed refs.
            P.Implicit -> Right diffs
    go s _ = Right s

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
        | decl /= newDecl -> (a, r, ref : c, u)
        | otherwise -> (a, r, c, ref : u)

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
    | P.isDictTypeName n -> Nothing
    | otherwise -> Just (TypeRef n, typeDeps t <> typeKindDeps tk)
  --
  P.EDTypeSynonym n args t ->
    Just (TypeRef n, typeDeps t <> foldArgs args)
  --
  P.EDDataConstructor n _ tn t _
    | P.isDictTypeName n -> Nothing
    | otherwise ->
        Just
          ( ConstructorRef tn n
          , -- Add the type as a dependency: if the type has changed (e.g. left side
            -- param is added) we should recompile the module which uses the
            -- constructor (even if there no the explicit type import).
            -- Also add the ad-hoc constructors set ref dependency: if a ctor
            -- added/removed it should affect all constructors in the type,
            -- because case statement's validity may be affected by newly added
            -- or removed constructors.
            typeDeps t <> S.fromList [(moduleName, TypeRef tn), (moduleName, CtorsSetRef tn)]
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


-- | Replace type arguments with ordered names, this allows to handle
-- generated arg names (like t43) and user's rename of args.
refineType :: P.Type P.SourceAnn -> P.Type P.SourceAnn
refineType = fst . flip runState M.empty .
  P.everywhereOnTypesM
    (\case
      P.ForAll ann vis var mbK ty sco -> getName var <&> \v -> P.ForAll ann vis v mbK ty sco
      P.TypeVar ann var -> getName var <&> P.TypeVar ann
      other -> pure other
    )
  where
  getName varName = do
    m <- get
    case M.lookup varName m of
      Just rep ->
        pure rep
      Nothing -> do
        let rep = "a" <> show (M.size m)
        put (M.insert varName rep m)
        pure rep

-- | Removes excessive info from declarations and rename type arguments before
-- comparing.
refineDeclaration :: P.ExternsDeclaration -> P.ExternsDeclaration
refineDeclaration = \case
  P.EDType n t k ->
    P.EDType n (refineType t) (refineTypeKind k)

  P.EDTypeSynonym n args t ->
    P.EDTypeSynonym n (refineArgs args) (refineType t)

  P.EDDataConstructor n org tn t ident ->
    P.EDDataConstructor n org tn (refineType t) ident

  P.EDValue n t ->
    P.EDValue n  (refineType t)

  P.EDClass n args mem con dep emt ->
    P.EDClass n (refineArgs args) (map (map refineType) mem) con dep emt

  P.EDInstance cn n fa ks ts cs ch chi ns _ss ->
    P.EDInstance cn n (map (map refineType) fa) (map refineType ks) (map refineType ts)
      cs (map refineChain ch) chi ns emptySpan

  where
    emptySpan =  P.SourceSpan "" emptySP emptySP
    emptySP = P.SourcePos 0 0
    refineChain (ChainId (_, _)) = ChainId ("", emptySP)
    refineArgs = map (map (map refineType))
    refineTypeKind = \case
      -- Remove the notion of data constructors, we only compare type's left side.
      P.DataType dt args _ -> P.DataType dt (refineDataArgs args) []
      other -> other
    refineDataArgs =
      zipWith (\idx (_, t, role) -> ("a" <> show idx, refineType <$> t, role)) [(0 :: Int)..]

isPrimModule :: ModuleName -> Bool
isPrimModule = flip S.member (S.fromList primModules)
