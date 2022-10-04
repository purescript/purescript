{- HLINT ignore "Unused LANGUAGE pragma" -} -- HLint doesn't recognize that TypeApplications is used in a pattern
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.TypeChecker.Deriving (deriveInstance) where

import Protolude hiding (Type)

import Control.Monad.Trans.Writer (Writer, WriterT, runWriter, runWriterT)
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Foldable (foldl1, foldr1)
import Data.List (init, last, zipWith3, (!!))
import qualified Data.Map as M

import Control.Monad.Supply.Class
import Language.PureScript.AST
import Language.PureScript.AST.Utils
import qualified Language.PureScript.Constants.Data.Foldable as Foldable
import qualified Language.PureScript.Constants.Data.Traversable as Traversable
import qualified Language.PureScript.Constants.Prelude as Prelude
import qualified Language.PureScript.Constants.Prim as Prim
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors hiding (nonEmpty)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names
import Language.PureScript.PSString
import Language.PureScript.Sugar.TypeClasses
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

-- | Extract the name of the newtype appearing in the last type argument of
-- a derived newtype instance.
--
-- Note: since newtypes in newtype instances can only be applied to type arguments
-- (no flexible instances allowed), we don't need to bother with unification when
-- looking for matching superclass instances, which saves us a lot of work. Instead,
-- we just match the newtype name.
extractNewtypeName :: ModuleName -> [SourceType] -> Maybe (ModuleName, ProperName 'TypeName)
extractNewtypeName mn
  = fmap (\(n, _, _) -> qualify mn n)
  . (unwrapTypeConstructor <=< lastMay)

deriveInstance
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => MonadWriter MultipleErrors m
  => SourceType
  -> Qualified (ProperName 'ClassName)
  -> InstanceDerivationStrategy
  -> m Expr
deriveInstance instType className strategy = do
  mn <- unsafeCheckCurrentModule
  env <- getEnv
  (fmap coerceProperName -> ctorName, _, tys) <- maybe (internalCompilerError "invalid instance type") pure $ unwrapTypeConstructor instType

  TypeClassData{..} <-
    note (errorMessage . UnknownName $ fmap TyClassName className) $
      className `M.lookup` typeClasses env

  case strategy of
    KnownClassStrategy -> let
      unaryClass :: (ModuleName -> ProperName 'TypeName -> m [(PSString, Expr)]) -> m Expr
      unaryClass f = case tys of
        [ty] -> case unwrapTypeConstructor ty of
          Just (Qualified (ByModuleName mn') tyCon, _, _) | mn == mn' -> do
            let superclassesDicts = flip map typeClassSuperclasses $ \(Constraint _ superclass _ suTyArgs _) ->
                  let tyArgs = map (replaceAllTypeVars (zip (map fst typeClassArguments) tys)) suTyArgs
                  in lam UnusedIdent (DeferredDictionary superclass tyArgs)
            let superclasses = map mkString (superClassDictionaryNames typeClassSuperclasses) `zip` superclassesDicts
            App (Constructor nullSourceSpan ctorName) . mkLit . ObjectLiteral . (++ superclasses) <$> f mn tyCon
          _ -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
        _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1

      unaryClass' f = unaryClass (f className)

      in case className of
        Foldable.Foldable -> unaryClass' deriveFoldable
        Prelude.Eq -> unaryClass deriveEq
        Prelude.Eq1 -> unaryClass $ \_ _ -> deriveEq1
        Prelude.Functor -> unaryClass' deriveFunctor
        Prelude.Ord -> unaryClass deriveOrd
        Prelude.Ord1 -> unaryClass $ \_ _ -> deriveOrd1
        Traversable.Traversable -> unaryClass' deriveTraversable
        -- See L.P.Sugar.TypeClasses.Deriving for the classes that can be
        -- derived prior to type checking.
        _ -> throwError . errorMessage $ CannotDerive className tys

    NewtypeStrategy ->
      case tys of
        _ : _ | Just (Qualified (ByModuleName mn') tyCon, kargs, args) <- unwrapTypeConstructor (last tys)
              , mn == mn'
              -> deriveNewtypeInstance mn className tys tyCon kargs args
              | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys (last tys)
        _ -> throwError . errorMessage $ InvalidNewtypeInstance className tys

deriveNewtypeInstance
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> Qualified (ProperName 'ClassName)
  -> [SourceType]
  -> ProperName 'TypeName
  -> [SourceType]
  -> [SourceType]
  -> m Expr
deriveNewtypeInstance mn className tys tyConNm dkargs dargs = do
    verifySuperclasses
    (dtype, tyKindNames, tyArgNames, ctors) <- lookupTypeDecl mn tyConNm
    go dtype tyKindNames tyArgNames ctors
  where
    go (Just Newtype) tyKindNames tyArgNames [(_, [wrapped])] = do
      -- The newtype might not be applied to all type arguments.
      -- This is okay as long as the newtype wraps something which ends with
      -- sufficiently many type applications to variables.
      -- For example, we can derive Functor for
      --
      -- newtype MyArray a = MyArray (Array a)
      --
      -- since Array a is a type application which uses the last
      -- type argument
      wrapped' <- replaceAllTypeSynonyms wrapped
      case stripRight (takeReverse (length tyArgNames - length dargs) tyArgNames) wrapped' of
        Just wrapped'' -> do
          let subst = zipWith (\(name, _) t -> (name, t)) tyArgNames dargs <> zip tyKindNames dkargs
          wrapped''' <- replaceAllTypeSynonyms $ replaceAllTypeVars subst wrapped''
          tys' <- mapM replaceAllTypeSynonyms tys
          return (DeferredDictionary className (init tys' ++ [wrapped''']))
        Nothing -> throwError . errorMessage $ InvalidNewtypeInstance className tys
    go _ _ _ _ = throwError . errorMessage $ InvalidNewtypeInstance className tys

    takeReverse :: Int -> [a] -> [a]
    takeReverse n = take n . reverse

    stripRight :: [(Text, Maybe kind)] -> SourceType -> Maybe SourceType
    stripRight [] ty = Just ty
    stripRight ((arg, _) : args) (TypeApp _ t (TypeVar _ arg'))
      | arg == arg' = stripRight args t
    stripRight _ _ = Nothing

    verifySuperclasses :: m ()
    verifySuperclasses = do
      env <- getEnv
      for_ (M.lookup className (typeClasses env)) $ \TypeClassData{ typeClassArguments = args, typeClassSuperclasses = superclasses } ->
        for_ superclasses $ \Constraint{..} -> do
          let constraintClass' = qualify (internalError "verifySuperclasses: unknown class module") constraintClass
          for_ (M.lookup constraintClass (typeClasses env)) $ \TypeClassData{ typeClassDependencies = deps } ->
            -- We need to check whether the newtype is mentioned, because of classes like MonadWriter
            -- with its Monoid superclass constraint.
            when (not (null args) && any ((fst (last args) `elem`) . usedTypeVariables) constraintArgs) $ do
              -- For now, we only verify superclasses where the newtype is the only argument,
              -- or for which all other arguments are determined by functional dependencies.
              -- Everything else raises a UnverifiableSuperclassInstance warning.
              -- This covers pretty much all cases we're interested in, but later we might want to do
              -- more work to extend this to other superclass relationships.
              let determined = map (srcTypeVar . fst . (args !!)) . ordNub . concatMap fdDetermined . filter ((== [length args - 1]) . fdDeterminers) $ deps
              if eqType (last constraintArgs) (srcTypeVar . fst $ last args) && all (`elem` determined) (init constraintArgs)
                then do
                  -- Now make sure that a superclass instance was derived. Again, this is not a complete
                  -- check, since the superclass might have multiple type arguments, so overlaps might still
                  -- be possible, so we warn again.
                  for_ (extractNewtypeName mn tys) $ \nm -> do
                    unless (hasNewtypeSuperclassInstance constraintClass' nm (typeClassDictionaries env)) $
                      tell . errorMessage $ MissingNewtypeSuperclassInstance constraintClass className tys
                else tell . errorMessage $ UnverifiableSuperclassInstance constraintClass className tys

    -- Note that this check doesn't actually verify that the superclass is
    -- newtype-derived; see #3168. The whole verifySuperclasses feature
    -- is pretty sketchy, and could use a thorough review and probably rewrite.
    hasNewtypeSuperclassInstance (suModule, suClass) nt@(newtypeModule, _) dicts =
      let su = Qualified (ByModuleName suModule) suClass
          lookIn mn'
            = elem nt
            . (toList . extractNewtypeName mn' . tcdInstanceTypes
                <=< foldMap toList . M.elems
                <=< toList . (M.lookup su <=< M.lookup (ByModuleName mn')))
            $ dicts
      in lookIn suModule || lookIn newtypeModule

deriveEq
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> ProperName 'TypeName
  -> m [(PSString, Expr)]
deriveEq mn tyConNm = do
  (_, _, _, ctors) <- lookupTypeDecl mn tyConNm
  eqFun <- mkEqFunction ctors
  pure [(Prelude.eq, eqFun)]
  where
    mkEqFunction :: [(ProperName 'ConstructorName, [SourceType])] -> m Expr
    mkEqFunction ctors = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 x y . addCatch <$> mapM mkCtorClause ctors

    preludeConj :: Expr -> Expr -> Expr
    preludeConj = App . App (mkVarMn (Just (ModuleName "Data.HeytingAlgebra")) (Ident Prelude.conj))

    preludeEq :: Expr -> Expr -> Expr
    preludeEq = App . App (mkRef Prelude.identEq)

    preludeEq1 :: Expr -> Expr -> Expr
    preludeEq1 = App . App (mkRef Prelude.identEq1)

    addCatch :: [CaseAlternative] -> [CaseAlternative]
    addCatch xs
      | length xs /= 1 = xs ++ [catchAll]
      | otherwise = xs -- Avoid redundant case
      where
      catchAll = CaseAlternative [NullBinder, NullBinder] (unguarded (mkLit (BooleanLiteral False)))

    mkCtorClause :: (ProperName 'ConstructorName, [SourceType]) -> m CaseAlternative
    mkCtorClause (ctorName, tys) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM replaceAllTypeSynonyms tys
      let tests = zipWith3 toEqTest (map mkVar identsL) (map mkVar identsR) tys'
      return $ CaseAlternative [caseBinder identsL, caseBinder identsR] (unguarded (conjAll tests))
      where
      caseBinder idents = mkCtorBinder mn ctorName $ map mkBinder idents

    conjAll :: [Expr] -> Expr
    conjAll = \case
      [] -> mkLit (BooleanLiteral True)
      xs -> foldl1 preludeConj xs

    toEqTest :: Expr -> Expr -> SourceType -> Expr
    toEqTest l r ty
      | Just fields <- decomposeRec <=< objectType $ ty
        = conjAll
        . map (\(Label str, typ) -> toEqTest (Accessor str l) (Accessor str r) typ)
        $ fields
      | isAppliedVar ty = preludeEq1 l r
      | otherwise = preludeEq l r

deriveEq1 :: forall m. Applicative m => m [(PSString, Expr)]
deriveEq1 = pure [(Prelude.eq1, mkRef Prelude.identEq)]

deriveOrd
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> ProperName 'TypeName
  -> m [(PSString, Expr)]
deriveOrd mn tyConNm = do
  (_, _, _, ctors) <- lookupTypeDecl mn tyConNm
  compareFun <- mkCompareFunction ctors
  pure [(Prelude.compare, compareFun)]
  where
    mkCompareFunction :: [(ProperName 'ConstructorName, [SourceType])] -> m Expr
    mkCompareFunction ctors = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 x y <$> (addCatch . concat <$> mapM mkCtorClauses (splitLast ctors))

    splitLast :: [a] -> [(a, Bool)]
    splitLast [] = []
    splitLast [x] = [(x, True)]
    splitLast (x : xs) = (x, False) : splitLast xs

    addCatch :: [CaseAlternative] -> [CaseAlternative]
    addCatch xs
      | null xs = [catchAll] -- No type constructors
      | otherwise = xs
      where
      catchAll = CaseAlternative [NullBinder, NullBinder] (unguarded (orderingCtor "EQ"))

    orderingMod :: ModuleName
    orderingMod = ModuleName "Data.Ordering"

    orderingCtor :: Text -> Expr
    orderingCtor = mkCtor orderingMod . ProperName

    orderingBinder :: Text -> Binder
    orderingBinder name = mkCtorBinder orderingMod (ProperName name) []

    ordCompare :: Expr -> Expr -> Expr
    ordCompare = App . App (mkRef Prelude.identCompare)

    ordCompare1 :: Expr -> Expr -> Expr
    ordCompare1 = App . App (mkRef Prelude.identCompare1)

    mkCtorClauses :: ((ProperName 'ConstructorName, [SourceType]), Bool) -> m [CaseAlternative]
    mkCtorClauses ((ctorName, tys), isLast) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM replaceAllTypeSynonyms tys
      let tests = zipWith3 toOrdering (map mkVar identsL) (map mkVar identsR) tys'
          extras | not isLast = [ CaseAlternative [nullCaseBinder, NullBinder] (unguarded (orderingCtor "LT"))
                                , CaseAlternative [NullBinder, nullCaseBinder] (unguarded (orderingCtor "GT"))
                                ]
                 | otherwise = []
      return $ CaseAlternative [ caseBinder identsL
                               , caseBinder identsR
                               ]
                               (unguarded (appendAll tests))
             : extras

      where
      caseBinder idents = mkCtorBinder mn ctorName $ map mkBinder idents
      nullCaseBinder = mkCtorBinder mn ctorName $ replicate (length tys) NullBinder

    appendAll :: [Expr] -> Expr
    appendAll = \case
      [] -> orderingCtor "EQ"
      [x] -> x
      (x : xs) -> Case [x] [ CaseAlternative [orderingBinder "LT"] (unguarded (orderingCtor "LT"))
                           , CaseAlternative [orderingBinder "GT"] (unguarded (orderingCtor "GT"))
                           , CaseAlternative [NullBinder] (unguarded (appendAll xs))
                           ]

    toOrdering :: Expr -> Expr -> SourceType -> Expr
    toOrdering l r ty
      | Just fields <- decomposeRec <=< objectType $ ty
        = appendAll
        . map (\(Label str, typ) -> toOrdering (Accessor str l) (Accessor str r) typ)
        $ fields
      | isAppliedVar ty = ordCompare1 l r
      | otherwise = ordCompare l r

deriveOrd1 :: forall m. Applicative m => m [(PSString, Expr)]
deriveOrd1 = pure [(Prelude.compare1, mkRef Prelude.identCompare)]

lookupTypeDecl
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => ModuleName
  -> ProperName 'TypeName
  -> m (Maybe DataDeclType, [Text], [(Text, Maybe SourceType)], [(ProperName 'ConstructorName, [SourceType])])
lookupTypeDecl mn typeName = do
  env <- getEnv
  note (errorMessage $ CannotFindDerivingType typeName) $ do
    (kind, DataType _ args dctors) <- Qualified (ByModuleName mn) typeName `M.lookup` types env
    (kargs, _) <- completeBinderList kind
    let dtype = do
          (ctorName, _) <- headMay dctors
          (a, _, _, _) <- Qualified (ByModuleName mn) ctorName `M.lookup` dataConstructors env
          pure a
    pure (dtype, fst . snd <$> kargs, map (\(v, k, _) -> (v, k)) args, dctors)

isAppliedVar :: Type a -> Bool
isAppliedVar (TypeApp _ (TypeVar _ _) _) = True
isAppliedVar _ = False

objectType :: Type a -> Maybe (Type a)
objectType (TypeApp _ (TypeConstructor _ Prim.Record) rec) = Just rec
objectType _ = Nothing

decomposeRec :: SourceType -> Maybe [(Label, SourceType)]
decomposeRec = fmap (sortOn fst) . go
  where go (RCons _ str typ typs) = fmap ((str, typ) :) (go typs)
        go (REmptyKinded _ _) = Just []
        go _ = Nothing

decomposeRec' :: SourceType -> [(Label, SourceType)]
decomposeRec' = sortOn fst . go
  where go (RCons _ str typ typs) = (str, typ) : go typs
        go _ = []

data ParamUsage
  = IsParam
  | MentionsParam ParamUsage
  | IsRecord (NonEmpty (PSString, ParamUsage))

validateParamsInTypeConstructors
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => Qualified (ProperName 'ClassName)
  -> ModuleName
  -> ProperName 'TypeName
  -> m [(ProperName 'ConstructorName, [Maybe ParamUsage])]
validateParamsInTypeConstructors derivingClass mn tyConNm = do
  (_, _, tyArgNames, ctors) <- lookupTypeDecl mn tyConNm
  param <- note (errorMessage $ KindsDoNotUnify (kindType -:> kindType) kindType) . lastMay $ map fst tyArgNames
  ctors' <- traverse (traverse $ traverse replaceAllTypeSynonyms) ctors
  let (ctorUsages, problemSpans) = runWriter $ traverse (traverse . traverse $ typeToUsageOf param) ctors'
  for_ (nonEmpty $ ordNub problemSpans) $ \sss ->
    throwError . addHint (RelatedPositions sss) . errorMessage $ CannotDeriveInvalidConstructorArg derivingClass
  pure ctorUsages
  where
  typeToUsageOf :: Text -> SourceType -> Writer [SourceSpan] (Maybe ParamUsage)
  typeToUsageOf param = go
    where
    assertNoParamUsedIn :: SourceType -> Writer [SourceSpan] ()
    assertNoParamUsedIn = everythingOnTypes (*>) $ \case
      TypeVar (ss, _) name | name == param -> tell [ss]
      _ -> pure ()

    go = \case
      ForAll _ name _ ty _ ->
        if name == param then pure Nothing else go ty

      ConstrainedType _ _ ty ->
        go ty

      TypeApp _ (TypeConstructor _ Prim.Record) row ->
        fmap (fmap IsRecord . nonEmpty . catMaybes) . for (decomposeRec' row) $ \(Label lbl, ty) ->
          fmap (lbl, ) <$> go ty

      TypeApp _ tyFn tyArg -> do
        assertNoParamUsedIn tyFn
        fmap MentionsParam <$> go tyArg

      TypeVar _ name ->
        pure $ (name == param) `orEmpty` IsParam

      ty ->
        assertNoParamUsedIn ty $> Nothing

usingLamIdent :: forall m. MonadSupply m => (Expr -> m Expr) -> m Expr
usingLamIdent cb = do
  ident <- freshIdent "v"
  lam ident <$> cb (mkVar ident)

traverseFields :: forall f. Applicative f => (ParamUsage -> Expr -> f Expr) -> NonEmpty (PSString, ParamUsage) -> Expr -> f Expr
traverseFields f fields r = fmap (ObjectUpdate r) . for (toList fields) $ \(lbl, usage) -> (lbl, ) <$> f usage (Accessor lbl r)

unnestRecords :: forall f. Applicative f => (ParamUsage -> Expr -> f Expr) -> ParamUsage -> Expr -> f Expr
unnestRecords f = fix $ \go -> \case
  IsRecord fields -> traverseFields go fields
  usage -> f usage

mkCasesForTraversal
  :: forall f m
   . Applicative f -- this effect distinguishes the semantics of maps, folds, and traversals
  => MonadSupply m
  => ModuleName
  -> (ParamUsage -> Expr -> f Expr) -- how to handle constructor arguments
  -> (f Expr -> m Expr) -- resolve the applicative effect into an expression
  -> [(ProperName 'ConstructorName, [Maybe ParamUsage])]
  -> m Expr
mkCasesForTraversal mn handleArg extractExpr ctors = do
  m <- freshIdent "m"
  fmap (lamCase m) . for ctors $ \(ctorName, ctorUsages) -> do
    ctorArgs <- for ctorUsages $ \usage -> freshIdent "v" <&> (, usage)
    let ctor = mkCtor mn ctorName
    let caseBinder = mkCtorBinder mn ctorName $ map (mkBinder . fst) ctorArgs
    fmap (CaseAlternative [caseBinder] . unguarded) . extractExpr $
      fmap (foldl' App ctor) . for ctorArgs $ \(ident, mbUsage) -> maybe pure handleArg mbUsage $ mkVar ident

data TraversalOps m = forall f. Applicative f => TraversalOps
  { visitExpr :: m Expr -> f Expr -- lift an expression into the applicative effect defining the traversal
  , extractExpr :: f Expr -> m Expr -- resolve the applicative effect into an expression
  }

mkTraversal
  :: forall m
   . MonadSupply m
  => ModuleName
  -> Expr -- a var representing map, foldMap, or traverse, for handling structured values
  -> TraversalOps m
  -> [(ProperName 'ConstructorName, [Maybe ParamUsage])]
  -> m Expr
mkTraversal mn recurseVar (TraversalOps @_ @f visitExpr extractExpr) ctors = do
  f <- freshIdent "f"
  let
    handleValue :: ParamUsage -> Expr -> f Expr
    handleValue = unnestRecords $ \usage inputExpr -> visitExpr $ flip App inputExpr <$> mkFnExprForValue usage

    mkFnExprForValue :: ParamUsage -> m Expr
    mkFnExprForValue = \case
      IsParam ->
        pure $ mkVar f
      MentionsParam innerUsage ->
        App recurseVar <$> mkFnExprForValue innerUsage
      IsRecord fields ->
        usingLamIdent $ extractExpr . traverseFields handleValue fields

  lam f <$> mkCasesForTraversal mn handleValue extractExpr ctors

deriveFunctor
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => Qualified (ProperName 'ClassName)
  -> ModuleName
  -> ProperName 'TypeName
  -> m [(PSString, Expr)]
deriveFunctor nm mn tyConNm = do
  ctors <- validateParamsInTypeConstructors nm mn tyConNm
  mapFun <- mkTraversal mn mapVar (TraversalOps identity identity) ctors
  pure [(Prelude.map, mapFun)]
  where
  mapVar = mkRef Prelude.identMap

toConst :: forall f a b. f a -> Const [f a] b
toConst = Const . pure

consumeConst :: forall f a b c. Applicative f => ([a] -> b) -> Const [f a] c -> f b
consumeConst f = fmap f . sequenceA . getConst

applyWhen :: forall a. Bool -> (a -> a) -> a -> a
applyWhen cond f = if cond then f else identity

deriveFoldable
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => Qualified (ProperName 'ClassName)
  -> ModuleName
  -> ProperName 'TypeName
  -> m [(PSString, Expr)]
deriveFoldable nm mn tyConNm = do
  ctors <- validateParamsInTypeConstructors nm mn tyConNm
  foldlFun <- mkAsymmetricFoldFunction False foldlVar ctors
  foldrFun <- mkAsymmetricFoldFunction True foldrVar ctors
  foldMapFun <- mkTraversal mn foldMapVar foldMapOps ctors
  pure [(Foldable.foldl, foldlFun), (Foldable.foldr, foldrFun), (Foldable.foldMap, foldMapFun)]
  where
  foldlVar = mkRef Foldable.identFoldl
  foldrVar = mkRef Foldable.identFoldr
  foldMapVar = mkRef Foldable.identFoldMap
  flipVar = mkRef Prelude.identFlip

  mkAsymmetricFoldFunction :: Bool -> Expr -> [(ProperName 'ConstructorName, [Maybe ParamUsage])] -> m Expr
  mkAsymmetricFoldFunction isRightFold recurseVar ctors = do
    f <- freshIdent "f"
    z <- freshIdent "z"
    let
      appCombiner :: (Bool, Expr) -> Expr -> Expr -> Expr
      appCombiner (isFlipped, fn) = applyWhen (isFlipped == isRightFold) flip $ App . App fn

      mkCombinerExpr :: ParamUsage -> m Expr
      mkCombinerExpr = fmap (uncurry $ \isFlipped -> applyWhen isFlipped $ App flipVar) . getCombiner

      handleValue :: ParamUsage -> Expr -> Const [m (Expr -> Expr)] Expr
      handleValue = unnestRecords $ \usage inputExpr -> toConst $ flip appCombiner inputExpr <$> getCombiner usage

      getCombiner :: ParamUsage -> m (Bool, Expr)
      getCombiner = \case
        IsParam ->
          pure (False, mkVar f)
        MentionsParam innerUsage ->
          (isRightFold, ) . App recurseVar <$> mkCombinerExpr innerUsage
        IsRecord fields -> do
          let foldFieldsOf = traverseFields handleValue fields
          fmap (False, ) . usingLamIdent $ \lVar ->
            usingLamIdent $
              if isRightFold
              then flip extractExprStartingWith $ foldFieldsOf lVar
              else extractExprStartingWith lVar . foldFieldsOf

      extractExprStartingWith :: Expr -> Const [m (Expr -> Expr)] Expr -> m Expr
      extractExprStartingWith = consumeConst . if isRightFold then foldr ($) else foldl' (&)

    lam f . lam z <$> mkCasesForTraversal mn handleValue (extractExprStartingWith $ mkVar z) ctors

foldMapOps :: forall m. Applicative m => TraversalOps m
foldMapOps = TraversalOps { visitExpr = toConst, .. }
  where
  appendVar = mkRef Prelude.identAppend
  memptyVar = mkRef Prelude.identMempty

  extractExpr :: Const [m Expr] Expr -> m Expr
  extractExpr = consumeConst $ \case
    [] -> memptyVar
    exprs -> foldr1 (App . App appendVar) exprs

deriveTraversable
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => Qualified (ProperName 'ClassName)
  -> ModuleName
  -> ProperName 'TypeName
  -> m [(PSString, Expr)]
deriveTraversable nm mn tyConNm = do
  ctors <- validateParamsInTypeConstructors nm mn tyConNm
  traverseFun <- mkTraversal mn traverseVar traverseOps ctors
  sequenceFun <- usingLamIdent $ pure . App (App traverseVar identityVar)
  pure [(Traversable.traverse, traverseFun), (Traversable.sequence, sequenceFun)]
  where
  traverseVar = mkRef Traversable.identTraverse
  identityVar = mkRef Prelude.identIdentity

traverseOps :: forall m. MonadSupply m => TraversalOps m
traverseOps = TraversalOps { .. }
  where
  pureVar = mkRef Prelude.identPure
  mapVar = mkRef Prelude.identMap
  applyVar = mkRef Prelude.identApply

  visitExpr :: m Expr -> WriterT [(Ident, m Expr)] m Expr
  visitExpr traversedExpr = do
    ident <- freshIdent "v"
    tell [(ident, traversedExpr)] $> mkVar ident

  extractExpr :: WriterT [(Ident, m Expr)] m Expr -> m Expr
  extractExpr = runWriterT >=> \(result, unzip -> (ctx, args)) -> flip mkApps (foldr lam result ctx) <$> sequenceA args

  mkApps :: [Expr] -> Expr -> Expr
  mkApps = \case
    [] -> App pureVar
    h : t -> \l -> foldl' (App . App applyVar) (App (App mapVar l) h) t
