{- HLINT ignore "Unused LANGUAGE pragma" -} -- HLint doesn't recognize that TypeApplications is used in a pattern
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Language.PureScript.TypeChecker.Deriving (deriveInstance) where

import Protolude hiding (Type)

import Control.Lens (both, over)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Writer (Writer, WriterT, runWriter, runWriterT)
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Align (align, unalign)
import Data.Foldable (foldl1, foldr1)
import Data.List (init, last, zipWith3, (!!))
import Data.Map qualified as M
import Data.These (These(..), mergeTheseWith, these)

import Language.PureScript.AST (Binder(..), CaseAlternative(..), ErrorMessageHint(..), Expr(..), InstanceDerivationStrategy(..), Literal(..), SourceSpan, nullSourceSpan)
import Language.PureScript.AST.Utils (UnwrappedTypeConstructor(..), lam, lamCase, lamCase2, mkBinder, mkCtor, mkCtorBinder, mkLit, mkRef, mkVar, unguarded, unwrapTypeConstructor, utcQTyCon)
import Language.PureScript.Constants.Libs qualified as Libs
import Language.PureScript.Constants.Prim qualified as Prim
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), FunctionalDependency(..), TypeClassData(..), TypeKind(..), kindType, (-:>))
import Language.PureScript.Errors (SimpleErrorMessage(..), addHint, errorMessage, internalCompilerError)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName(..), Name(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, freshIdent, qualify)
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Sugar.TypeClasses (superClassDictionaryNames)
import Language.PureScript.TypeChecker.Entailment (InstanceContext, findDicts)
import Language.PureScript.TypeChecker.Monad (getEnv, getTypeClassDictionaries, unsafeCheckCurrentModule, TypeCheckM)
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeClassDictionaries (TypeClassDictionaryInScope(..))
import Language.PureScript.Types (Constraint(..), pattern REmptyKinded, SourceType, Type(..), completeBinderList, eqType, everythingOnTypes, replaceAllTypeVars, srcTypeVar, usedTypeVariables)

-- | Extract the name of the newtype appearing in the last type argument of
-- a derived newtype instance.
--
-- Note: since newtypes in newtype instances can only be applied to type arguments
-- (no flexible instances allowed), we don't need to bother with unification when
-- looking for matching superclass instances, which saves us a lot of work. Instead,
-- we just match the newtype name.
extractNewtypeName :: ModuleName -> [SourceType] -> Maybe (ModuleName, ProperName 'TypeName)
extractNewtypeName mn
  = fmap (qualify mn . utcQTyCon)
  . (unwrapTypeConstructor <=< lastMay)

deriveInstance
  :: SourceType
  -> Qualified (ProperName 'ClassName)
  -> InstanceDerivationStrategy
  -> TypeCheckM Expr
deriveInstance instType className strategy = do
  mn <- unsafeCheckCurrentModule
  env <- getEnv
  instUtc@UnwrappedTypeConstructor{ utcArgs = tys } <- maybe (internalCompilerError "invalid instance type") pure $ unwrapTypeConstructor instType
  let ctorName = coerceProperName <$> utcQTyCon instUtc

  TypeClassData{..} <-
    note (errorMessage . UnknownName $ fmap TyClassName className) $
      className `M.lookup` typeClasses env

  case strategy of
    KnownClassStrategy -> let
      unaryClass :: (UnwrappedTypeConstructor -> TypeCheckM [(PSString, Expr)]) -> TypeCheckM Expr
      unaryClass f = case tys of
        [ty] -> case unwrapTypeConstructor ty of
          Just utc | mn == utcModuleName utc -> do
            let superclassesDicts = flip map typeClassSuperclasses $ \(Constraint _ superclass _ suTyArgs _) ->
                  let tyArgs = map (replaceAllTypeVars (zip (map fst typeClassArguments) tys)) suTyArgs
                  in lam UnusedIdent (DeferredDictionary superclass tyArgs)
            let superclasses = map mkString (superClassDictionaryNames typeClassSuperclasses) `zip` superclassesDicts
            App (Constructor nullSourceSpan ctorName) . mkLit . ObjectLiteral . (++ superclasses) <$> f utc
          _ -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
        _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1

      unaryClass' f = unaryClass (f className)

      in case className of
        Libs.Bifoldable -> unaryClass' $ deriveFoldable True
        Libs.Bifunctor -> unaryClass' $ deriveFunctor (Just False) False Libs.S_bimap
        Libs.Bitraversable -> unaryClass' $ deriveTraversable True
        Libs.Contravariant -> unaryClass' $ deriveFunctor Nothing True Libs.S_cmap
        Libs.Eq -> unaryClass deriveEq
        Libs.Eq1 -> unaryClass $ const deriveEq1
        Libs.Foldable -> unaryClass' $ deriveFoldable False
        Libs.Functor -> unaryClass' $ deriveFunctor Nothing False Libs.S_map
        Libs.Ord -> unaryClass deriveOrd
        Libs.Ord1 -> unaryClass $ const deriveOrd1
        Libs.Profunctor -> unaryClass' $ deriveFunctor (Just True) False Libs.S_dimap
        Libs.Traversable -> unaryClass' $ deriveTraversable False
        -- See L.P.Sugar.TypeClasses.Deriving for the classes that can be
        -- derived prior to type checking.
        _ -> throwError . errorMessage $ CannotDerive className tys

    NewtypeStrategy ->
      case tys of
        _ : _ | Just utc <- unwrapTypeConstructor (last tys)
              , mn == utcModuleName utc
              -> deriveNewtypeInstance className tys utc
              | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys (last tys)
        _ -> throwError . errorMessage $ InvalidNewtypeInstance className tys

deriveNewtypeInstance
  :: Qualified (ProperName 'ClassName)
  -> [SourceType]
  -> UnwrappedTypeConstructor
  -> TypeCheckM Expr
deriveNewtypeInstance className tys (UnwrappedTypeConstructor mn tyConNm dkargs dargs) = do
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

    verifySuperclasses :: TypeCheckM ()
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

data TypeInfo = TypeInfo
  { tiTypeParams :: [Text]
  , tiCtors :: [(ProperName 'ConstructorName, [SourceType])]
  , tiArgSubst :: [(Text, SourceType)]
  }

lookupTypeInfo
  :: UnwrappedTypeConstructor
  -> TypeCheckM TypeInfo
lookupTypeInfo UnwrappedTypeConstructor{..} = do
  (_, kindParams, map fst -> tiTypeParams, tiCtors) <- lookupTypeDecl utcModuleName utcTyCon
  let tiArgSubst = zip tiTypeParams utcArgs <> zip kindParams utcKindArgs
  pure TypeInfo{..}

deriveEq
  :: UnwrappedTypeConstructor
  -> TypeCheckM [(PSString, Expr)]
deriveEq utc = do
  TypeInfo{..} <- lookupTypeInfo utc
  eqFun <- mkEqFunction tiCtors
  pure [(Libs.S_eq, eqFun)]
  where
    mkEqFunction :: [(ProperName 'ConstructorName, [SourceType])] -> TypeCheckM Expr
    mkEqFunction ctors = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 x y . addCatch <$> mapM mkCtorClause ctors

    preludeConj :: Expr -> Expr -> Expr
    preludeConj = App . App (mkRef Libs.I_conj)

    preludeEq :: Expr -> Expr -> Expr
    preludeEq = App . App (mkRef Libs.I_eq)

    preludeEq1 :: Expr -> Expr -> Expr
    preludeEq1 = App . App (mkRef Libs.I_eq1)

    addCatch :: [CaseAlternative] -> [CaseAlternative]
    addCatch xs
      | length xs /= 1 = xs ++ [catchAll]
      | otherwise = xs -- Avoid redundant case
      where
      catchAll = CaseAlternative [NullBinder, NullBinder] (unguarded (mkLit (BooleanLiteral False)))

    mkCtorClause :: (ProperName 'ConstructorName, [SourceType]) -> TypeCheckM CaseAlternative
    mkCtorClause (ctorName, tys) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM replaceAllTypeSynonyms tys
      let tests = zipWith3 toEqTest (map mkVar identsL) (map mkVar identsR) tys'
      return $ CaseAlternative [caseBinder identsL, caseBinder identsR] (unguarded (conjAll tests))
      where
      caseBinder idents = mkCtorBinder (utcModuleName utc) ctorName $ map mkBinder idents

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
deriveEq1 = pure [(Libs.S_eq1, mkRef Libs.I_eq)]

deriveOrd
  :: UnwrappedTypeConstructor
  -> TypeCheckM [(PSString, Expr)]
deriveOrd utc = do
  TypeInfo{..} <- lookupTypeInfo utc
  compareFun <- mkCompareFunction tiCtors
  pure [(Libs.S_compare, compareFun)]
  where
    mkCompareFunction :: [(ProperName 'ConstructorName, [SourceType])] -> TypeCheckM Expr
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
    ordCompare = App . App (mkRef Libs.I_compare)

    ordCompare1 :: Expr -> Expr -> Expr
    ordCompare1 = App . App (mkRef Libs.I_compare1)

    mkCtorClauses :: ((ProperName 'ConstructorName, [SourceType]), Bool) -> TypeCheckM [CaseAlternative]
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
      mn = utcModuleName utc
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
deriveOrd1 = pure [(Libs.S_compare1, mkRef Libs.I_compare)]

lookupTypeDecl
  :: ModuleName
  -> ProperName 'TypeName
  -> TypeCheckM (Maybe DataDeclType, [Text], [(Text, Maybe SourceType)], [(ProperName 'ConstructorName, [SourceType])])
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

-- | The parameter `c` is used to allow or forbid contravariance for different
-- type classes. When deriving a type class that is a variation on Functor, a
-- witness for `c` will be provided; when deriving a type class that is a
-- variation on Foldable or Traversable, `c` will be Void and the contravariant
-- ParamUsage constructor can be skipped in pattern matching.
data ParamUsage c
  = IsParam
  | IsLParam
    -- ^ enables biparametric classes (of any variance) to be derived
  | MentionsParam (ParamUsage c)
    -- ^ enables monoparametric classes to be used in a derivation
  | MentionsParamBi (These (ParamUsage c) (ParamUsage c))
    -- ^ enables biparametric classes to be used in a derivation
  | MentionsParamContravariantly !c (ContravariantParamUsage c)
    -- ^ enables contravariant classes (of either parametricity) to be used in a derivation
  | IsRecord (NonEmpty (PSString, ParamUsage c))

data ContravariantParamUsage c
  = MentionsParamContra (ParamUsage c)
    -- ^ enables Contravariant to be used in a derivation
  | MentionsParamPro (These (ParamUsage c) (ParamUsage c))
    -- ^ enables Profunctor to be used in a derivation

data CovariantClasses = CovariantClasses
  { monoClass :: Qualified (ProperName 'ClassName)
  , biClass :: Qualified (ProperName 'ClassName)
  }

data ContravariantClasses = ContravariantClasses
  { contraClass :: Qualified (ProperName 'ClassName)
  , proClass :: Qualified (ProperName 'ClassName)
  }

data ContravarianceSupport c = ContravarianceSupport
  { contravarianceWitness :: c
  , paramIsContravariant :: Bool
  , lparamIsContravariant :: Bool
  , contravariantClasses :: ContravariantClasses
  }

-- | Return, if possible, a These the contents of which each satisfy the
-- predicate.
filterThese :: forall a. (a -> Bool) -> These a a -> Maybe (These a a)
filterThese p = uncurry align . over both (mfilter p) . unalign . Just

validateParamsInTypeConstructors
  :: forall c
   . Qualified (ProperName 'ClassName)
  -> UnwrappedTypeConstructor
  -> Bool
  -> CovariantClasses
  -> Maybe (ContravarianceSupport c)
  -> TypeCheckM [(ProperName 'ConstructorName, [Maybe (ParamUsage c)])]
validateParamsInTypeConstructors derivingClass utc isBi CovariantClasses{..} contravarianceSupport = do
  TypeInfo{..} <- lookupTypeInfo utc
  (mbLParam, param) <- liftEither . first (errorMessage . flip KindsDoNotUnify kindType . (kindType -:>)) $
    case (isBi, reverse tiTypeParams) of
      (False, x : _)    -> Right (Nothing, x)
      (False, _)        -> Left kindType
      (True, y : x : _) -> Right (Just x, y)
      (True, _ : _)     -> Left kindType
      (True, _)         -> Left $ kindType -:> kindType
  ctors <- traverse (traverse $ traverse replaceAllTypeSynonyms) tiCtors
  tcds <- getTypeClassDictionaries
  let (ctorUsages, problemSpans) = runWriter $ traverse (traverse . traverse $ typeToUsageOf tcds tiArgSubst (maybe That These mbLParam param) False) ctors
  let relatedClasses = [monoClass, biClass] ++ ([contraClass, proClass] <*> (contravariantClasses <$> toList contravarianceSupport))
  for_ (nonEmpty $ ordNub problemSpans) $ \sss ->
    throwError . addHint (RelatedPositions sss) . errorMessage $ CannotDeriveInvalidConstructorArg derivingClass relatedClasses (isJust contravarianceSupport)
  pure ctorUsages

  where
  typeToUsageOf :: InstanceContext -> [(Text, SourceType)] -> These Text Text -> Bool -> SourceType -> Writer [SourceSpan] (Maybe (ParamUsage c))
  typeToUsageOf tcds subst = fix $ \go params isNegative -> let
    goCo = go params isNegative
    goContra = go params $ not isNegative

    assertNoParamUsedIn :: SourceType -> Writer [SourceSpan] ()
    assertNoParamUsedIn ty = void $ both (flip assertParamNotUsedIn ty) params

    assertParamNotUsedIn :: Text -> SourceType -> Writer [SourceSpan] ()
    assertParamNotUsedIn param = everythingOnTypes (*>) $ \case
      TypeVar (ss, _) name | name == param -> tell [ss]
      _ -> pure ()

    tryBiClasses ht tyLArg tyArg
      | hasInstance tcds ht biClass
        = goCo tyLArg >>= preferMonoClass MentionsParamBi
      | Just (ContravarianceSupport c _ _ ContravariantClasses{..}) <- contravarianceSupport, hasInstance tcds ht proClass
        = goContra tyLArg >>= preferMonoClass (MentionsParamContravariantly c . MentionsParamPro)
      | otherwise
        = assertNoParamUsedIn tyLArg *> tryMonoClasses ht tyArg
      where
      preferMonoClass f lUsage =
        (if isNothing lUsage && hasInstance tcds ht monoClass then fmap MentionsParam else fmap f . align lUsage) <$> goCo tyArg

    tryMonoClasses ht tyArg
      | hasInstance tcds ht monoClass
        = fmap MentionsParam <$> goCo tyArg
      | Just (ContravarianceSupport c _ _ ContravariantClasses{..}) <- contravarianceSupport, hasInstance tcds ht contraClass
        = fmap (MentionsParamContravariantly c . MentionsParamContra) <$> goContra tyArg
      | otherwise
        = assertNoParamUsedIn tyArg $> Nothing

    headOfTypeWithSubst :: SourceType -> Qualified (Either Text (ProperName 'TypeName))
    headOfTypeWithSubst = headOfType . replaceAllTypeVars subst

    in \case
      ForAll _ _ name _ ty _ ->
        fmap join . traverse (\params' -> go params' isNegative ty) $ filterThese (/= name) params

      ConstrainedType _ _ ty ->
        goCo ty

      TypeApp _ (TypeConstructor _ Prim.Record) row ->
        fmap (fmap IsRecord . nonEmpty . catMaybes) . for (decomposeRec' row) $ \(Label lbl, ty) ->
          fmap (lbl, ) <$> goCo ty

      TypeApp _ (TypeApp _ tyFn tyLArg) tyArg ->
        assertNoParamUsedIn tyFn *> tryBiClasses (headOfTypeWithSubst tyFn) tyLArg tyArg

      TypeApp _ tyFn tyArg ->
        assertNoParamUsedIn tyFn *> tryMonoClasses (headOfTypeWithSubst tyFn) tyArg

      TypeVar (ss, _) name -> mergeTheseWith (checkName lparamIsContra IsLParam) (checkName paramIsContra IsParam) (liftA2 (<|>)) params
        where
        checkName thisParamIsContra usage param
          | name == param = when (thisParamIsContra /= isNegative) (tell [ss]) $> Just usage
          | otherwise = pure Nothing

      ty ->
        assertNoParamUsedIn ty $> Nothing

  paramIsContra = any paramIsContravariant contravarianceSupport
  lparamIsContra = any lparamIsContravariant contravarianceSupport

  hasInstance :: InstanceContext -> Qualified (Either Text (ProperName 'TypeName)) -> Qualified (ProperName 'ClassName) -> Bool
  hasInstance tcds ht@(Qualified qb _) cn@(Qualified cqb _) =
    any tcdAppliesToType $ concatMap (findDicts tcds cn) (ordNub [ByNullSourcePos, cqb, qb])
    where
    tcdAppliesToType tcd = case tcdInstanceTypes tcd of
      [headOfType -> ht'] -> ht == ht'
        -- It's possible that, if ht and ht' are Lefts, this might require
        -- verifying that the name isn't shadowed by something in tcdForAll. I
        -- can't devise a legal program that causes this issue, but if in the
        -- future it seems like a good idea, it probably is.
      _ -> False

  headOfType :: SourceType -> Qualified (Either Text (ProperName 'TypeName))
  headOfType = fix $ \go -> \case
    TypeApp _ ty _ -> go ty
    KindApp _ ty _ -> go ty
    TypeVar _ nm -> Qualified ByNullSourcePos (Left nm)
    Skolem _ nm _ _ _ -> Qualified ByNullSourcePos (Left nm)
    TypeConstructor _ (Qualified qb nm) -> Qualified qb (Right nm)
    ty -> internalError $ "headOfType missing a case: " <> show (void ty)

usingLamIdent :: (Expr -> TypeCheckM Expr) -> TypeCheckM Expr
usingLamIdent cb = do
  ident <- freshIdent "v"
  lam ident <$> cb (mkVar ident)

traverseFields :: forall c f. Applicative f => (ParamUsage c -> Expr -> f Expr) -> NonEmpty (PSString, ParamUsage c) -> Expr -> f Expr
traverseFields f fields r = fmap (ObjectUpdate r) . for (toList fields) $ \(lbl, usage) -> (lbl, ) <$> f usage (Accessor lbl r)

unnestRecords :: forall c f. Applicative f => (ParamUsage c -> Expr -> f Expr) -> ParamUsage c -> Expr -> f Expr
unnestRecords f = fix $ \go -> \case
  IsRecord fields -> traverseFields go fields
  usage -> f usage

mkCasesForTraversal
  :: Applicative f
  => ModuleName
  -> (ParamUsage c -> Expr -> f Expr) -- how to handle constructor arguments
  -> (f Expr -> TypeCheckM Expr) -- resolve the applicative effect into an expression
  -> [(ProperName 'ConstructorName, [Maybe (ParamUsage c)])]
  -> TypeCheckM Expr
mkCasesForTraversal mn handleArg extractExpr ctors = do
  m <- freshIdent "m"
  fmap (lamCase m) . for ctors $ \(ctorName, ctorUsages) -> do
    ctorArgs <- for ctorUsages $ \usage -> freshIdent "v" <&> (, usage)
    let ctor = mkCtor mn ctorName
    let caseBinder = mkCtorBinder mn ctorName $ map (mkBinder . fst) ctorArgs
    fmap (CaseAlternative [caseBinder] . unguarded) . extractExpr $
      fmap (foldl' App ctor) . for ctorArgs $ \(ident, mbUsage) -> maybe pure handleArg mbUsage $ mkVar ident

data TraversalExprs = TraversalExprs
  { recurseVar :: Expr -- a var representing map, foldMap, or traverse, for handling structured values
  , birecurseVar :: Expr -- same, but bimap, bifoldMap, or bitraverse
  , lrecurseExpr :: Expr -- same, but lmap or ltraverse (there is no lfoldMap, but we can use `flip bifoldMap mempty`)
  , rrecurseExpr :: Expr -- same, but rmap or rtraverse etc., which conceptually should be the same as recurseVar but the bi classes aren't subclasses of the mono classes
  }

data ContraversalExprs = ContraversalExprs
  { crecurseVar :: Expr
  , direcurseVar :: Expr
  , lcrecurseVar :: Expr
  , rprorecurseVar :: Expr
  }

appBirecurseExprs :: TraversalExprs -> These Expr Expr -> Expr
appBirecurseExprs TraversalExprs{..} = these (App lrecurseExpr) (App rrecurseExpr) (App . App birecurseVar)

appDirecurseExprs :: ContraversalExprs -> These Expr Expr -> Expr
appDirecurseExprs ContraversalExprs{..} = these (App lcrecurseVar) (App rprorecurseVar) (App . App direcurseVar)

data TraversalOps m = forall f. Applicative f => TraversalOps
  { visitExpr :: m Expr -> f Expr -- lift an expression into the applicative effect defining the traversal
  , extractExpr :: f Expr -> m Expr -- resolve the applicative effect into an expression
  }

mkTraversal
  :: forall c. ModuleName
  -> Bool
  -> TraversalExprs
  -> (c -> ContraversalExprs)
  -> TraversalOps TypeCheckM
  -> [(ProperName 'ConstructorName, [Maybe (ParamUsage c)])]
  -> TypeCheckM Expr
mkTraversal mn isBi te@TraversalExprs{..} getContraversalExprs (TraversalOps @_ @f visitExpr extractExpr) ctors = do
  f <- freshIdent "f"
  g <- if isBi then freshIdent "g" else pure f
  let
    handleValue :: ParamUsage c -> Expr -> f Expr
    handleValue = unnestRecords $ \usage inputExpr -> visitExpr $ flip App inputExpr <$> mkFnExprForValue usage

    mkFnExprForValue :: ParamUsage c -> TypeCheckM Expr
    mkFnExprForValue = \case
      IsParam ->
        pure $ mkVar g
      IsLParam ->
        pure $ mkVar f
      MentionsParam innerUsage ->
        App recurseVar <$> mkFnExprForValue innerUsage
      MentionsParamBi theseInnerUsages ->
        appBirecurseExprs te <$> both mkFnExprForValue theseInnerUsages
      MentionsParamContravariantly c contraUsage -> do
        let ce@ContraversalExprs{..} = getContraversalExprs c
        case contraUsage of
          MentionsParamContra innerUsage ->
            App crecurseVar <$> mkFnExprForValue innerUsage
          MentionsParamPro theseInnerUsages ->
            appDirecurseExprs ce <$> both mkFnExprForValue theseInnerUsages
      IsRecord fields ->
        usingLamIdent $ extractExpr . traverseFields handleValue fields

  lam f . applyWhen isBi (lam g) <$> mkCasesForTraversal mn handleValue extractExpr ctors

deriveFunctor
  :: Maybe Bool -- does left parameter exist, and is it contravariant?
  -> Bool -- is the (right) parameter contravariant?
  -> PSString -- name of the map function for this functor type
  -> Qualified (ProperName 'ClassName)
  -> UnwrappedTypeConstructor
  -> TypeCheckM [(PSString, Expr)]
deriveFunctor mbLParamIsContravariant paramIsContravariant mapName nm utc = do
  ctors <- validateParamsInTypeConstructors nm utc isBi functorClasses $ Just $ ContravarianceSupport
    { contravarianceWitness = ()
    , paramIsContravariant
    , lparamIsContravariant = or mbLParamIsContravariant
    , contravariantClasses
    }
  mapFun <- mkTraversal (utcModuleName utc) isBi mapExprs (const cmapExprs) (TraversalOps identity identity) ctors
  pure [(mapName, mapFun)]
  where
  isBi = isJust mbLParamIsContravariant
  mapExprs = TraversalExprs
    { recurseVar = mkRef Libs.I_map
    , birecurseVar = mkRef Libs.I_bimap
    , lrecurseExpr = mkRef Libs.I_lmap
    , rrecurseExpr = mkRef Libs.I_rmap
    }
  cmapExprs = ContraversalExprs
    { crecurseVar = mkRef Libs.I_cmap
    , direcurseVar = mkRef Libs.I_dimap
    , lcrecurseVar = mkRef Libs.I_lcmap
    , rprorecurseVar = mkRef Libs.I_profunctorRmap
    }
  functorClasses = CovariantClasses Libs.Functor Libs.Bifunctor
  contravariantClasses = ContravariantClasses Libs.Contravariant Libs.Profunctor

toConst :: forall f a b. f a -> Const [f a] b
toConst = Const . pure

consumeConst :: forall f a b c. Applicative f => ([a] -> b) -> Const [f a] c -> f b
consumeConst f = fmap f . sequenceA . getConst

applyWhen :: forall a. Bool -> (a -> a) -> a -> a
applyWhen cond f = if cond then f else identity

deriveFoldable
  :: Bool -- is there a left parameter (are we deriving Bifoldable)?
  -> Qualified (ProperName 'ClassName)
  -> UnwrappedTypeConstructor
  -> TypeCheckM [(PSString, Expr)]
deriveFoldable isBi nm utc = do
  ctors <- validateParamsInTypeConstructors nm utc isBi foldableClasses Nothing
  foldlFun <- mkAsymmetricFoldFunction False foldlExprs ctors
  foldrFun <- mkAsymmetricFoldFunction True foldrExprs ctors
  foldMapFun <- mkTraversal mn isBi foldMapExprs absurd foldMapOps ctors
  pure
    [ (if isBi then Libs.S_bifoldl else Libs.S_foldl, foldlFun)
    , (if isBi then Libs.S_bifoldr else Libs.S_foldr, foldrFun)
    , (if isBi then Libs.S_bifoldMap else Libs.S_foldMap, foldMapFun)
    ]
  where
  mn = utcModuleName utc
  foldableClasses = CovariantClasses Libs.Foldable Libs.Bifoldable
  foldlExprs = TraversalExprs
    { recurseVar = mkRef Libs.I_foldl
    , birecurseVar = bifoldlVar
    , lrecurseExpr = App (App flipVar bifoldlVar) constVar
    , rrecurseExpr = App bifoldlVar constVar
    }
  foldrExprs = TraversalExprs
    { recurseVar = mkRef Libs.I_foldr
    , birecurseVar = bifoldrVar
    , lrecurseExpr = App (App flipVar bifoldrVar) (App constVar identityVar)
    , rrecurseExpr = App bifoldrVar (App constVar identityVar)
    }
  foldMapExprs = TraversalExprs
    { recurseVar = mkRef Libs.I_foldMap
    , birecurseVar = bifoldMapVar
    , lrecurseExpr = App (App flipVar bifoldMapVar) memptyVar
    , rrecurseExpr = App bifoldMapVar memptyVar
    }
  bifoldlVar = mkRef Libs.I_bifoldl
  bifoldrVar = mkRef Libs.I_bifoldr
  bifoldMapVar = mkRef Libs.I_bifoldMap
  constVar = mkRef Libs.I_const
  flipVar = mkRef Libs.I_flip
  identityVar = mkRef Libs.I_identity
  memptyVar = mkRef Libs.I_mempty

  mkAsymmetricFoldFunction :: Bool -> TraversalExprs -> [(ProperName 'ConstructorName, [Maybe (ParamUsage Void)])] -> TypeCheckM Expr
  mkAsymmetricFoldFunction isRightFold te@TraversalExprs{..} ctors = do
    f <- freshIdent "f"
    g <- if isBi then freshIdent "g" else pure f
    z <- freshIdent "z"
    let
      appCombiner :: (Bool, Expr) -> Expr -> Expr -> Expr
      appCombiner (isFlipped, fn) = applyWhen (isFlipped == isRightFold) flip $ App . App fn

      mkCombinerExpr :: ParamUsage Void -> TypeCheckM Expr
      mkCombinerExpr = fmap (uncurry $ \isFlipped -> applyWhen isFlipped $ App flipVar) . getCombiner

      handleValue :: ParamUsage Void -> Expr -> Const [TypeCheckM (Expr -> Expr)] Expr
      handleValue = unnestRecords $ \usage inputExpr -> toConst $ flip appCombiner inputExpr <$> getCombiner usage

      getCombiner :: ParamUsage Void -> TypeCheckM (Bool, Expr)
      getCombiner = \case
        IsParam ->
          pure (False, mkVar g)
        IsLParam ->
          pure (False, mkVar f)
        MentionsParam innerUsage ->
          (isRightFold, ) . App recurseVar <$> mkCombinerExpr innerUsage
        MentionsParamBi theseInnerUsages ->
          (isRightFold, ) . appBirecurseExprs te <$> both mkCombinerExpr theseInnerUsages
        IsRecord fields -> do
          let foldFieldsOf = traverseFields handleValue fields
          fmap (False, ) . usingLamIdent $ \lVar ->
            usingLamIdent $
              if isRightFold
              then flip extractExprStartingWith $ foldFieldsOf lVar
              else extractExprStartingWith lVar . foldFieldsOf

      extractExprStartingWith :: Expr -> Const [TypeCheckM (Expr -> Expr)] Expr -> TypeCheckM Expr
      extractExprStartingWith = consumeConst . if isRightFold then foldr ($) else foldl' (&)

    lam f . applyWhen isBi (lam g) . lam z <$> mkCasesForTraversal mn handleValue (extractExprStartingWith $ mkVar z) ctors

foldMapOps :: forall m. Applicative m => TraversalOps m
foldMapOps = TraversalOps { visitExpr = toConst, .. }
  where
  appendVar = mkRef Libs.I_append
  memptyVar = mkRef Libs.I_mempty

  extractExpr :: Const [m Expr] Expr -> m Expr
  extractExpr = consumeConst $ \case
    [] -> memptyVar
    exprs -> foldr1 (App . App appendVar) exprs

deriveTraversable
  :: Bool -- is there a left parameter (are we deriving Bitraversable)?
  -> Qualified (ProperName 'ClassName)
  -> UnwrappedTypeConstructor
  -> TypeCheckM [(PSString, Expr)]
deriveTraversable isBi nm utc = do
  ctors <- validateParamsInTypeConstructors nm utc isBi traversableClasses Nothing
  traverseFun <- mkTraversal (utcModuleName utc) isBi traverseExprs absurd traverseOps ctors
  sequenceFun <- usingLamIdent $ pure . App (App (if isBi then App bitraverseVar identityVar else traverseVar) identityVar)
  pure
    [ (if isBi then Libs.S_bitraverse else Libs.S_traverse, traverseFun)
    , (if isBi then Libs.S_bisequence else Libs.S_sequence, sequenceFun)
    ]
  where
  traversableClasses = CovariantClasses Libs.Traversable Libs.Bitraversable
  traverseExprs = TraversalExprs
    { recurseVar = traverseVar
    , birecurseVar = bitraverseVar
    , lrecurseExpr = mkRef Libs.I_ltraverse
    , rrecurseExpr = mkRef Libs.I_rtraverse
    }
  traverseVar = mkRef Libs.I_traverse
  bitraverseVar = mkRef Libs.I_bitraverse
  identityVar = mkRef Libs.I_identity

traverseOps :: TraversalOps TypeCheckM
traverseOps = TraversalOps { .. }
  where
  pureVar = mkRef Libs.I_pure
  mapVar = mkRef Libs.I_map
  applyVar = mkRef Libs.I_apply

  visitExpr :: TypeCheckM Expr -> WriterT [(Ident, TypeCheckM Expr)] TypeCheckM Expr
  visitExpr traversedExpr = do
    ident <- freshIdent "v"
    tell [(ident, traversedExpr)] $> mkVar ident

  extractExpr :: WriterT [(Ident, TypeCheckM Expr)] TypeCheckM Expr -> TypeCheckM Expr
  extractExpr = runWriterT >=> \(result, unzip -> (ctx, args)) -> flip mkApps (foldr lam result ctx) <$> sequenceA args

  mkApps :: [Expr] -> Expr -> Expr
  mkApps = \case
    [] -> App pureVar
    h : t -> \l -> foldl' (App . App applyVar) (App (App mapVar l) h) t
