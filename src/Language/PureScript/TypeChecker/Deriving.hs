module Language.PureScript.TypeChecker.Deriving (deriveInstance) where

-- import Protolude hiding (Type)
import Protolude hiding (Type, traceM)

import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Foldable (foldl1)
import Data.List (init, last, zipWith3, (!!))
import qualified Data.List.NonEmpty as NonEmpty
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

      validateFFTArgs
        :: ProperName 'TypeName
        -> (ModuleName -> [(ProperName 'ConstructorName, [Maybe (FftArg ())])] -> m [(PSString, Expr)])
        -> m [(PSString, Expr)]
      validateFFTArgs tyConNm f = do
        (_, _, argTypes, ctors) <- lookupTypeDecl mn tyConNm
        case reverse argTypes of
          [] ->
            throwError . errorMessage $ KindsDoNotUnify (kindType -:> kindType) kindType
          ((iTy, _) : _) -> do
            ctors' <- traverse (traverse (traverse replaceAllTypeSynonyms)) ctors
            case unaryFftCheckCtorValidity iTy ctors' of
              Left invalidUsages -> do
                throwError $ fold $ do
                  iu <- invalidUsages
                  (withHints, ss) <- sortOn snd $ toList iu
                  pure
                    $ addHint (ErrorInInstance className tys)
                    $ withHints
                    $ errorMessage $ CannotDeriveInvalidConstructorArg iTy ss
              Right validCtors -> do
                f mn validCtors

      in case className of
        Prelude.Eq -> unaryClass deriveEq
        Prelude.Eq1 -> unaryClass $ \_ _ -> deriveEq1
        Prelude.Ord -> unaryClass deriveOrd
        Prelude.Ord1 -> unaryClass $ \_ _ -> deriveOrd1
        Prelude.Functor -> unaryClass $ \_ tyConNm -> validateFFTArgs tyConNm deriveFunctor
        Foldable.Foldable -> unaryClass $ \_ tyConNm -> validateFFTArgs tyConNm deriveFoldable
        Traversable.Traversable -> unaryClass $ \_ tyConNm -> validateFFTArgs tyConNm deriveTraversable
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

decomposeRecSorted :: SourceType -> [(PSString, SourceType)]
decomposeRecSorted = sortOn fst . go []
  where go !acc (RCons _ str typ typs) = go ((runLabel str, typ) : acc) typs
        go !acc _ = acc

data FftArg a
  = FftBaseArg a
  | FftRecursiveArg a (FftArg ())
  | FftRecordArg (NonEmpty (PSString, FftArg a))
  deriving (Show, Functor, Foldable, Traversable)

unaryFftCheckCtorValidity
  :: Text
  -> [(ProperName 'ConstructorName, [SourceType])]
  -> Either
      [NonEmpty (MultipleErrors -> MultipleErrors, SourceSpan)]
      [(ProperName 'ConstructorName, [Maybe (FftArg ())])]
unaryFftCheckCtorValidity iTy ctors = do
  let
    (ctorLefts, ctorRights) = foldl' (\(ctorLs, ctorRs) (ctor, subterms) -> do
      let
        (subLefts, subRights) = foldl' (
            \(ls, rs) relevance -> case unaryFunctorArgs iTy relevance of
              Left xs -> (toList xs : ls, rs)
              Right arg -> (ls, arg : rs)
          ) ([], []) subterms
      case nonEmpty $ join $ reverse subLefts of
        Just subLefts' -> (map (first (map (addHint (ErrorInDataConstructor ctor)))) subLefts' : ctorLs, ctorRs)
        Nothing -> (ctorLs, (ctor, reverse subRights) : ctorRs)
      ) ([], []) ctors
  if null ctorLefts then Right $ reverse ctorRights else Left $ reverse ctorLefts

unaryFunctorArgs
  :: Text -- ^ The type var for which we're looking (e.g. @c@ in @data Foo a b c = ...@)
  -> SourceType -- ^ the data constructor arg's type
  -> Either (NonEmpty (MultipleErrors -> MultipleErrors, SourceSpan)) (Maybe (FftArg ()))
unaryFunctorArgs iTyName = goTypeNested
  where
  goTypeNested :: SourceType -> Either (NonEmpty (MultipleErrors -> MultipleErrors, SourceSpan)) (Maybe (FftArg ()))
  goTypeNested = \case
    -- quantifiers
    (ForAll _ scopedVar _ t _) | scopedVar /= iTyName ->
      goTypeNested t

    -- constraints
    (ConstrainedType _ _ t) ->
      goTypeNested t

    -- bare index type
    TypeVar _ t | t == iTyName ->
      pure $ Just $ FftBaseArg ()

    TypeApp _ (TypeConstructor _ Prim.Record) rows -> do
      let
        (errors, validRows) = partitionEithers $ decomposeRecSorted rows <&> \next -> do
          bimap (map (first (map (addHint (ErrorUnderLabel $ fst next))))) sequence $ traverse goTypeNested next
      case nonEmpty errors of
        Just errs ->
          Left $ join errs
        Nothing ->
          pure $ map FftRecordArg $ nonEmpty $ catMaybes validRows

    (TypeApp _ tf ta) -> do
      case nonEmpty $ hasInvalidUsage False False tf of
        Nothing ->
          map (map (FftRecursiveArg ())) $ goTypeNested ta
        Just invalidUsageIndices ->
          Left invalidUsageIndices

    -- otherwise invalid arg
    _ -> do
      pure Nothing

  hasInvalidUsage :: Bool -> Bool -> SourceType -> [(MultipleErrors -> MultipleErrors, SourceSpan)]
  hasInvalidUsage reportRightmostTyParam isRightmostTyParam = \case
    (ForAll _ scopedVar _ t _) | scopedVar /= iTyName ->
      hasInvalidUsage reportRightmostTyParam isRightmostTyParam t

    (ConstrainedType _ _ t) ->
      hasInvalidUsage reportRightmostTyParam isRightmostTyParam t
    
    TypeVar ann t
      | t == iTyName
      , isRightmostTyParam
      , reportRightmostTyParam -> [ (identity, fst ann) ]
      
      | t == iTyName -> [ (identity, fst ann) ]
      
      | otherwise -> []

    TypeApp _ (TypeConstructor _ Prim.Record) rows -> do
      decomposeRecSorted rows >>= (hasInvalidUsage reportRightmostTyParam True . snd)

    TypeApp _ tf ta -> do
      hasInvalidUsage reportRightmostTyParam False tf <> hasInvalidUsage reportRightmostTyParam isRightmostTyParam ta

    _ -> []

unaryMkCtorClauseWith
  :: forall m
   . MonadSupply m
  => ModuleName
  -> (Expr -> [(Ident, Maybe (FftArg ()))] -> m Expr)
  -> (ProperName 'ConstructorName, [Maybe (FftArg ())])
  -> m CaseAlternative
unaryMkCtorClauseWith mn buildExpr (ctorName, ctorArgs) = do
  subterms <- traverse (\arg -> (,arg) <$> freshIdent "v") ctorArgs
  let
    idents = map fst subterms
    ctor = mkCtor mn ctorName
    caseBinder = mkCtorBinder mn ctorName $ map mkBinder idents
  finalExpr <- buildExpr ctor subterms
  return (CaseAlternative [caseBinder] (unguarded finalExpr))

usingLamIdent :: forall m. MonadSupply m => (Expr -> m Expr) -> m Expr
usingLamIdent cb = do
  x <- freshIdent "v"
  lam x <$> cb (mkVar x)

deriveFunctor
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> [(ProperName 'ConstructorName, [Maybe (FftArg ())])]
  -> m [(PSString, Expr)]
deriveFunctor mn validCtors = do
    mapFn <- mkMapFn
    pure [(Prelude.map, mapFn)]
  where
    mapVar = mkRef Prelude.identMap

    mkMapFn = do
      f <- freshIdent "f"
      m <- freshIdent "m"
      let
        fVar = mkVar f

        buildExpr :: Expr -> [(Ident, Maybe (FftArg ()))] -> m Expr
        buildExpr ctorExpr ctorArgs = do
          args <- for ctorArgs $ \(caseBinderIdent, nextArg) -> do
            let cbIdent = mkVar caseBinderIdent
            mbArg <- traverse (mapArgExpr cbIdent) nextArg
            pure $ fromMaybe cbIdent mbArg
          pure $ foldl' App ctorExpr args

        mapArgExpr :: Expr -> FftArg () -> m Expr
        mapArgExpr argIdent = \case
          FftBaseArg _ -> do
            pure $ App fVar argIdent
          FftRecursiveArg _ subArg -> do
            fnExpr <- mapRecursiveArg subArg
            pure $ App (App mapVar fnExpr) argIdent
          FftRecordArg fields -> do
            fields' <- for fields $ \(lbl, fieldArg) ->
              (lbl,) <$> mapArgExpr (Accessor lbl argIdent) fieldArg
            pure $ ObjectUpdate argIdent $ toList fields'

        mapRecursiveArg :: FftArg () -> m Expr
        mapRecursiveArg = \case
          FftBaseArg _ ->
            pure fVar
          FftRecursiveArg _ subArg -> do
            fnExpr <- mapRecursiveArg subArg
            pure $ App mapVar fnExpr
          FftRecordArg fields -> do
            usingLamIdent $ \xIdent -> do
              fields' <- for fields $ \(lbl, fieldArg) ->
                (lbl,) <$> mapArgExpr (Accessor lbl xIdent) fieldArg
              pure $ ObjectUpdate xIdent $ toList fields'

      lam f . lamCase m <$> traverse (unaryMkCtorClauseWith mn buildExpr) validCtors

deriveFoldable
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> [(ProperName 'ConstructorName, [Maybe (FftArg ())])]
  -> m [(PSString, Expr)]
deriveFoldable mn validCtors = do
  foldlFn <- mkFoldlFn
  foldrFn <- mkFoldrFn
  foldMapFn <- mkFoldMapFn
  pure [(Foldable.foldl, foldlFn), (Foldable.foldr, foldrFn), (Foldable.foldMap, foldMapFn)]
  where
    foldlVar = mkRef Foldable.identFoldl
    foldrVar = mkRef Foldable.identFoldr
    foldMapVar = mkRef Foldable.identFoldMap
    appendVar = mkRef Prelude.identAppend
    memptyVar = mkRef Prelude.identMempty
    flipVar = mkRef Prelude.identFlip

    mkFoldlFn = do
      f <- freshIdent "f"
      z <- freshIdent "z"
      m <- freshIdent "m"
      let
        fVar = mkVar f
        zVar = mkVar z

        buildExpr :: Expr -> [(Ident, Maybe (FftArg ()))] -> m Expr
        buildExpr _ ctorArgs =
          foldl' (\acc (ident, arg) -> foldArg acc (mkVar ident) arg) (pure zVar) $ mapMaybe sequence ctorArgs

        foldArg :: m Expr -> Expr -> FftArg () -> m Expr
        foldArg accExpr identExpr = \case
          FftBaseArg _ -> do
            acc <- accExpr
            pure $ App (App fVar acc) identExpr
          FftRecursiveArg _ subArg -> do
            acc <- accExpr
            fn <- foldRecursiveArg subArg
            pure $ App (App (App foldlVar fn) acc) identExpr
          FftRecordArg fields -> do
            foldl' (\acc (lbl, arg) -> foldArg acc (Accessor lbl identExpr) arg) accExpr fields

        foldRecursiveArg :: FftArg () -> m Expr
        foldRecursiveArg = \case
          FftBaseArg _ -> do
            pure fVar
          FftRecursiveArg _ subArg -> do
            usingLamIdent $ \accIdent -> do
              fn <- foldRecursiveArg subArg
              pure $ App (App foldlVar fn) accIdent
          FftRecordArg fields -> do
            usingLamIdent $ \accIdent -> do
              usingLamIdent $ \recIdent -> do
                foldl' (\acc (lbl, arg) -> foldArg acc (Accessor lbl recIdent) arg) (pure accIdent) fields

      lam f . lam z . lamCase m <$> traverse (unaryMkCtorClauseWith mn buildExpr) validCtors

    mkFoldrFn = do
      f <- freshIdent "f"
      z <- freshIdent "z"
      m <- freshIdent "m"
      let
        fVar = mkVar f
        zVar = mkVar z

        buildExpr :: Expr -> [(Ident, Maybe (FftArg ()))] -> m Expr
        buildExpr _ ctorArgs =
          foldr (\(ident, arg) acc -> foldArg acc (mkVar ident) arg) (pure zVar) $ mapMaybe sequence ctorArgs

        foldArg :: m Expr -> Expr -> FftArg () -> m Expr
        foldArg accExpr identExpr = \case
          FftBaseArg _ -> do
            App (App fVar identExpr) <$> accExpr
          FftRecursiveArg _ subArg -> do
            acc <- accExpr
            fn <- foldRecursiveArg subArg
            pure $ App (App (App foldrVar fn) acc) identExpr
          FftRecordArg fields -> do
            foldr' (\(lbl, arg) acc -> foldArg acc (Accessor lbl identExpr) arg) accExpr fields

        foldRecursiveArg :: FftArg () -> m Expr
        foldRecursiveArg = \case
          FftBaseArg _ -> do
            pure fVar
          FftRecursiveArg _ subArg -> do
            usingLamIdent $ \accIdent -> do
              fn <- foldRecursiveArg subArg
              pure $ App (App flipVar (App foldrVar fn)) accIdent
          FftRecordArg fields -> do
            usingLamIdent $ \recIdent -> do
              usingLamIdent $ \accIdent -> do
                foldr' (\(lbl, arg) acc -> foldArg acc (Accessor lbl recIdent) arg) (pure accIdent) fields

      lam f . lam z . lamCase m <$> traverse (unaryMkCtorClauseWith mn buildExpr) validCtors

    mkFoldMapFn = do
      f <- freshIdent "f"
      m <- freshIdent "m"
      let
        fVar = mkVar f

        buildExpr :: Expr -> [(Ident, Maybe (FftArg ()))] -> m Expr
        buildExpr _ ctorArgs = case nonEmpty $ mapMaybe sequence ctorArgs of
          Nothing ->
            pure memptyVar
          Just args ->
            foldl1 appendArgs . join <$> traverse (\(ident, arg) -> foldArgs (mkVar ident) arg) args

        foldArgs :: Expr -> FftArg () -> m (NonEmpty Expr)
        foldArgs identExpr = \case
          FftBaseArg _ -> do
            pure $ App fVar identExpr :| []
          FftRecursiveArg _ subArg -> do
            fn <- foldRecursiveArg subArg
            pure $ App (App foldMapVar fn) identExpr :| []
          FftRecordArg fields -> do
            map join $ traverse (\(lbl, arg) -> foldArgs (Accessor lbl identExpr) arg) fields

        foldRecursiveArg :: FftArg () -> m Expr
        foldRecursiveArg = \case
          FftBaseArg _ -> do
            pure fVar
          FftRecursiveArg _ subArg -> do
            fn <- foldRecursiveArg subArg
            pure $ App foldMapVar fn
          FftRecordArg fields -> do
            usingLamIdent $ \xIdent -> do
              map (foldl1 appendArgs . join) $ traverse (\(lbl, arg) -> foldArgs (Accessor lbl xIdent) arg) fields

        appendArgs :: Expr -> Expr -> Expr
        appendArgs l r = App (App appendVar l) r

      lam f . lamCase m <$> traverse (unaryMkCtorClauseWith mn buildExpr) validCtors

deriveTraversable
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> [(ProperName 'ConstructorName, [Maybe (FftArg ())])]
  -> m [(PSString, Expr)]
deriveTraversable mn validCtors = do
  traverseFn <- mkTraverseFn validCtors
  sequenceFn <- mkSequenceFn
  pure [(Traversable.traverse, traverseFn), (Traversable.sequence, sequenceFn)]
  where
    traverseVar = mkRef Traversable.identTraverse
    identityVar = mkRef Prelude.identIdentity
    mapVar = mkRef Prelude.identMap
    applyVar = mkRef Prelude.identApply
    pureVar = mkRef Prelude.identPure

    mkSequenceFn = do
      t <- freshIdent "t"
      pure $ lam t $ App (App traverseVar identityVar) (mkVar t)

    mkTraverseFn ctors = do
      f <- freshIdent "f"
      t <- freshIdent "t"
      let
        fVar = mkVar f

        buildExpr :: Expr -> [(Ident, Maybe (FftArg ()))] -> m Expr
        buildExpr ctor ctorArgs = do
          let
            relevantArgs :: [(Ident, FftArg ())]
            relevantArgs = mapMaybe sequence ctorArgs
          traversedArgsList <- for relevantArgs $ \(ident, arg) -> do
            traverseNormalArg fVar (mkVar ident) arg
          case traversedArgsList >>= toList :: [Expr] of
            [] ->
              pure $ App pureVar $ foldl' (\acc -> App acc . mkVar . fst) ctor ctorArgs
            h:tl -> do
              genLamIdents ctorArgs $ \ctorArgs' -> do
                let ctorExpr = rebuildCtorExpr ctor ctorArgs'
                let lamExpr = foldr' (\ident body' -> lam ident body') ctorExpr $ foldMap extractIdents $ mapMaybe snd ctorArgs'
                pure $ foldl' (\acc -> App (App applyVar acc)) (App (App mapVar lamExpr) h) tl

      lam f . lamCase t <$> traverse (unaryMkCtorClauseWith mn buildExpr) ctors

    genLamIdents :: [(Ident, Maybe (FftArg ()))] -> ([(Ident, Maybe (FftArg Ident))] -> m Expr) -> m Expr
    genLamIdents ctorArgs useLamArgsCb = do
      foldr'
        (\arg cb argIdents -> do
            arg' <- traverse (traverse genIdents) arg
            cb $ arg' : argIdents
        )
        (\argIdents ->
          useLamArgsCb $ reverse argIdents
        )
        ctorArgs
        []

    genIdents :: FftArg () -> m (FftArg Ident)
    genIdents = traverse (const $ freshIdent "v")

    extractIdents :: FftArg Ident -> [Ident]
    extractIdents = foldMap (: [])

    rebuildCtorExpr :: Expr -> [(Ident, Maybe (FftArg Ident))] -> Expr
    rebuildCtorExpr ctorExpr args =
      foldl' (\acc arg -> App acc $ foldTopArg arg) ctorExpr args
      where
        foldTopArg :: (Ident, Maybe (FftArg Ident)) -> Expr
        foldTopArg (caseBinderIdent, arg) = case arg of
          Nothing -> mkVar caseBinderIdent
          Just tArg -> rebuildCtorArg (mkVar caseBinderIdent) tArg

    rebuildCtorArg :: Expr -> FftArg Ident -> Expr
    rebuildCtorArg identExpr = \case
      FftBaseArg lamIdent ->
        mkVar lamIdent
      FftRecursiveArg lamIdent _ ->
        mkVar lamIdent
      FftRecordArg fields -> do
        rebuildRecordArg identExpr fields

    rebuildRecordArg :: Expr -> NonEmpty (PSString, FftArg Ident) -> Expr
    rebuildRecordArg identExpr fields =
      ObjectUpdate identExpr $ foldMap (\(lbl, arg) -> [(lbl, rebuildCtorArg (Accessor lbl identExpr) arg)]) fields

    traverseNormalArg :: Expr -> Expr -> FftArg () -> m (NonEmpty Expr)
    traverseNormalArg fVar accessorExpr = \case
      FftBaseArg _ ->
        pure $ App fVar accessorExpr :| []
      FftRecursiveArg _ subArg -> do
        fn <- traverseRecursiveArg fVar subArg
        pure $ App (App traverseVar fn) accessorExpr :| []
      FftRecordArg fields ->
        map join $ for fields $ \(lbl, fieldArg) ->
          traverseNormalArg fVar (Accessor lbl accessorExpr) fieldArg

    traverseRecursiveArg :: Expr -> FftArg () -> m Expr
    traverseRecursiveArg fVar = \case
      FftBaseArg _ ->
        pure fVar
      FftRecursiveArg _ subArg -> do
        fn <- traverseRecursiveArg fVar subArg
        pure $ App traverseVar fn
      FftRecordArg fields -> do
        usingLamIdent $ \xIdent -> do
          fieldArgs <- map join $ for fields $ \(lbl, fieldArg) -> do
            traverseNormalArg fVar (Accessor lbl xIdent) fieldArg
          genRecordFieldLamIdents fields $ \fieldsWithIdents -> do
            let
              ctorExpr = rebuildRecordArg xIdent fieldsWithIdents
              lamExpr = foldr' (\ident body' -> lam ident body') ctorExpr $ foldMap (extractIdents . snd) fieldsWithIdents
            pure $ foldl' (\acc -> App (App applyVar acc)) (App (App mapVar lamExpr) (NonEmpty.head fieldArgs)) (NonEmpty.tail fieldArgs)

    genRecordFieldLamIdents :: NonEmpty (PSString, FftArg ()) -> (NonEmpty (PSString, FftArg Ident) -> m Expr) -> m Expr
    genRecordFieldLamIdents recFields useLamArgsCb = do
      foldr'
        (\arg cb argIdents -> do
          arg' <- traverse genIdents arg
          cb $ arg' : argIdents
        )
        (\argIdents -> case nonEmpty argIdents of
          Nothing ->
            internalCompilerError "genRecordFieldLamIdents: Impossible. Backwards traverse over non-empty list produced empty list."
          Just a ->
            useLamArgsCb $ NonEmpty.reverse a
        )
        recFields
        []
