module Language.PureScript.TypeChecker.Deriving (deriveInstance) where

import Protolude hiding (Type)

import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Foldable (foldl1)
import Data.List (init, last, zipWith3, (!!))
import qualified Data.Map as M

import Control.Monad.Supply.Class
import Language.PureScript.AST
import Language.PureScript.AST.Utils
import qualified Language.PureScript.Constants.Prelude as Prelude
import qualified Language.PureScript.Constants.Prim as Prim
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
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
          Just (Qualified (Just mn') tyCon, _, _) | mn == mn' -> do
            let superclassesDicts = flip map typeClassSuperclasses $ \(Constraint _ superclass _ suTyArgs _) ->
                  let tyArgs = map (replaceAllTypeVars (zip (map fst typeClassArguments) tys)) suTyArgs
                  in lam UnusedIdent (DeferredDictionary superclass tyArgs)
            let superclasses = map mkString (superClassDictionaryNames typeClassSuperclasses) `zip` superclassesDicts
            App (Constructor nullSourceSpan ctorName) . mkLit . ObjectLiteral . (++ superclasses) <$> f mn tyCon
          _ -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
        _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1

      in case className of
        Prelude.Eq -> unaryClass deriveEq
        Prelude.Eq1 -> unaryClass $ \_ _ -> deriveEq1
        Prelude.Functor -> unaryClass deriveFunctor
        Prelude.Ord -> unaryClass deriveOrd
        Prelude.Ord1 -> unaryClass $ \_ _ -> deriveOrd1
        -- See L.P.Sugar.TypeClasses.Deriving for the classes that can be
        -- derived prior to type checking.
        _ -> throwError . errorMessage $ CannotDerive className tys

    NewtypeStrategy ->
      case tys of
        _ : _ | Just (Qualified (Just mn') tyCon, kargs, args) <- unwrapTypeConstructor (last tys)
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
      let su = Qualified (Just suModule) suClass
          lookIn mn'
            = elem nt
            . (toList . extractNewtypeName mn' . tcdInstanceTypes
                <=< foldMap toList . M.elems
                <=< toList . (M.lookup su <=< M.lookup (Just mn')))
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
    (kind, DataType _ args dctors) <- Qualified (Just mn) typeName `M.lookup` types env
    (kargs, _) <- completeBinderList kind
    let dtype = do
          (ctorName, _) <- headMay dctors
          (a, _, _, _) <- Qualified (Just mn) ctorName `M.lookup` dataConstructors env
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

deriveFunctor
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => MonadSupply m
  => ModuleName
  -> ProperName 'TypeName
  -> m [(PSString, Expr)]
deriveFunctor mn tyConNm = do
  (_, _, tys, ctors) <- lookupTypeDecl mn tyConNm
  mapFun <- mkMapFunction tys ctors
  pure [(Prelude.map, mapFun)]
  where
    mkMapFunction :: [(Text, Maybe SourceType)] -> [(ProperName 'ConstructorName, [SourceType])] -> m Expr
    mkMapFunction tys ctors = case reverse tys of
      [] -> throwError . errorMessage $ KindsDoNotUnify (kindType -:> kindType) kindType
      ((iTy, _) : _) -> do
        f <- freshIdent "f"
        m <- freshIdent "m"
        lam f . lamCase m <$> mapM (mkCtorClause iTy f) ctors

    mkCtorClause :: Text -> Ident -> (ProperName 'ConstructorName, [SourceType]) -> m CaseAlternative
    mkCtorClause iTyName f (ctorName, ctorTys) = do
      idents <- replicateM (length ctorTys) (freshIdent "v")
      ctorTys' <- mapM replaceAllTypeSynonyms ctorTys
      args <- zipWithM transformArg idents ctorTys'
      let ctor = mkCtor mn ctorName
          rebuilt = foldl' App ctor args
          caseBinder = mkCtorBinder mn ctorName $ map mkBinder idents
      return $ CaseAlternative [caseBinder] (unguarded rebuilt)
      where
        fVar = mkVar f
        mapVar = mkRef Prelude.identMap

        transformArg :: Ident -> SourceType -> m Expr
        transformArg ident = fmap (foldr App (mkVar ident)) . goType where

          goType :: SourceType -> m (Maybe Expr)
          -- argument matches the index type
          goType (TypeVar _ t) | t == iTyName = return (Just fVar)

          -- records
          goType recTy | Just row <- objectType recTy =
              traverse buildUpdate (decomposeRec' row) >>= (traverse buildRecord . justUpdates)
            where
              justUpdates :: [Maybe (Label, Expr)] -> Maybe [(Label, Expr)]
              justUpdates = foldMap (fmap return)

              buildUpdate :: (Label, SourceType) -> m (Maybe (Label, Expr))
              buildUpdate (lbl, ty) = do upd <- goType ty
                                         return ((lbl,) <$> upd)

              buildRecord :: [(Label, Expr)] -> m Expr
              buildRecord updates = do
                arg <- freshIdent "o"
                let argVar = mkVar arg
                    mkAssignment (Label l, x) = (l, App x (Accessor l argVar))
                return (lam arg (ObjectUpdate argVar (mkAssignment <$> updates)))

          -- quantifiers
          goType (ForAll _ scopedVar _ t _) | scopedVar /= iTyName = goType t

          -- constraints
          goType (ConstrainedType _ _ t) = goType t

          -- under a `* -> *`, just assume functor for now
          goType (TypeApp _ _ t) = fmap (App mapVar) <$> goType t

          -- otherwise do nothing - will fail type checking if type does actually contain index
          goType _ = return Nothing
