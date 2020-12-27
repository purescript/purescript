-- | This module implements the generic deriving elaboration that takes place during desugaring.
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import           Prelude.Compat
import           Protolude (ordNub)

import           Control.Arrow (second)
import           Control.Monad (replicateM, zipWithM, unless, when)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Control.Monad.Supply.Class (MonadSupply)
import           Data.Foldable (for_)
import           Data.List (foldl', find, sortBy, unzip5)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Text (Text)
import           Language.PureScript.AST
import qualified Language.PureScript.Constants.Data.Generic.Rep as DataGenericRep
import qualified Language.PureScript.Constants.Data.Newtype as DataNewtype
import qualified Language.PureScript.Constants.Prelude as Prelude
import qualified Language.PureScript.Constants.Prim as Prim
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           Language.PureScript.Label (Label(..))
import           Language.PureScript.PSString (mkString)
import           Language.PureScript.Types
import           Language.PureScript.TypeChecker (checkNewtype)
import           Language.PureScript.TypeChecker.Synonyms (SynonymMap, KindMap, replaceAllTypeSynonymsM)

-- | When deriving an instance for a newtype, we must ensure that all superclass
-- instances were derived in the same way. This data structure is used to ensure
-- this property.
data NewtypeDerivedInstances = NewtypeDerivedInstances
  { ndiClasses :: M.Map (ModuleName, ProperName 'ClassName) ([Text], [SourceConstraint], [FunctionalDependency])
  -- ^ A list of superclass constraints for each type class. Since type classes
  -- have not been desugared here, we need to track this.
  , ndiDerivedInstances :: S.Set ((ModuleName, ProperName 'ClassName), (ModuleName, ProperName 'TypeName))
  -- ^ A list of newtype instances which were derived in this module.
  } deriving Show

instance Semigroup NewtypeDerivedInstances where
  x <> y =
    NewtypeDerivedInstances { ndiClasses          = ndiClasses          x <> ndiClasses          y
                            , ndiDerivedInstances = ndiDerivedInstances x <> ndiDerivedInstances y
                            }

instance Monoid NewtypeDerivedInstances where
  mempty = NewtypeDerivedInstances mempty mempty

-- | Extract the name of the newtype appearing in the last type argument of
-- a derived newtype instance.
--
-- Note: since newtypes in newtype instances can only be applied to type arguments
-- (no flexible instances allowed), we don't need to bother with unification when
-- looking for matching superclass instances, which saves us a lot of work. Instead,
-- we just match the newtype name.
extractNewtypeName :: ModuleName -> [SourceType] -> Maybe (ModuleName, ProperName 'TypeName)
extractNewtypeName _ [] = Nothing
extractNewtypeName mn xs = go (last xs) where
  go (TypeApp _ ty (TypeVar _ _)) = go ty
  go (TypeConstructor _ name) = Just (qualify mn name)
  go _ = Nothing

-- | Elaborates deriving instance declarations by code generation.
deriveInstances
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadSupply m)
  => [ExternsFile]
  -> SynonymMap
  -> KindMap
  -> Module
  -> m Module
deriveInstances externs syns kinds (Module ss coms mn ds exts) =
    Module ss coms mn <$> mapM (deriveInstance mn syns kinds instanceData ds) ds <*> pure exts
  where
    instanceData :: NewtypeDerivedInstances
    instanceData =
        foldMap (\ExternsFile{..} -> foldMap (fromExternsDecl efModuleName) efDeclarations) externs <> foldMap fromLocalDecl ds
      where
        fromExternsDecl mn' EDClass{..} =
          NewtypeDerivedInstances (M.singleton (mn', edClassName) (map fst edClassTypeArguments, edClassConstraints, edFunctionalDependencies)) mempty
        fromExternsDecl mn' EDInstance{..} =
          foldMap (\nm -> NewtypeDerivedInstances mempty (S.singleton (qualify mn' edInstanceClassName, nm))) (extractNewtypeName mn' edInstanceTypes)
        fromExternsDecl _ _ = mempty

        fromLocalDecl (TypeClassDeclaration _ cl args cons deps _) =
          NewtypeDerivedInstances (M.singleton (mn, cl) (map fst args, cons, deps)) mempty
        fromLocalDecl (TypeInstanceDeclaration _ _ _ _ _ cl tys _) =
          foldMap (\nm -> NewtypeDerivedInstances mempty (S.singleton (qualify mn cl, nm))) (extractNewtypeName mn tys)
        fromLocalDecl _ = mempty

-- | Takes a declaration, and if the declaration is a deriving TypeInstanceDeclaration,
-- elaborates that into an instance declaration via code generation.
deriveInstance
  :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> KindMap
  -> NewtypeDerivedInstances
  -> [Declaration]
  -> Declaration
  -> m Declaration
deriveInstance mn syns kinds _ ds (TypeInstanceDeclaration sa@(ss, _) ch idx nm deps className tys DerivedInstance)
  | className == Qualified (Just dataEq) (ProperName "Eq")
  = case tys of
      [ty] | Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration sa ch idx nm deps className tys . ExplicitInstance <$> deriveEq ss mn syns kinds ds tyCon
           | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataEq) (ProperName "Eq1")
  = case tys of
      [ty] | Just (Qualified mn' _, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> pure . TypeInstanceDeclaration sa ch idx nm deps className tys . ExplicitInstance $ deriveEq1 ss
           | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataOrd) (ProperName "Ord")
  = case tys of
      [ty] | Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration sa ch idx nm deps className tys . ExplicitInstance <$> deriveOrd ss mn syns kinds ds tyCon
           | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataOrd) (ProperName "Ord1")
  = case tys of
      [ty] | Just (Qualified mn' _, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> pure . TypeInstanceDeclaration sa ch idx nm deps className tys . ExplicitInstance $ deriveOrd1 ss
           | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataFunctor) (ProperName "Functor")
  = case tys of
      [ty] | Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration sa ch idx nm deps className tys . ExplicitInstance <$> deriveFunctor ss mn syns kinds ds tyCon
           | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 1
  | className == DataNewtype.Newtype
  = case tys of
      [wrappedTy, unwrappedTy]
        | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor wrappedTy
        , mn == fromMaybe mn mn'
        -> do actualUnwrappedTy <- deriveNewtype ss syns kinds ds tyCon args unwrappedTy
              return $ TypeInstanceDeclaration sa ch idx nm deps className [wrappedTy, actualUnwrappedTy] (ExplicitInstance [])
        | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys wrappedTy
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 2
  | className == DataGenericRep.Generic
  = case tys of
      [actualTy, repTy]
        | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor actualTy
        , mn == fromMaybe mn mn'
        -> do (inst, inferredRepTy) <- deriveGenericRep ss mn syns kinds ds tyCon args repTy
              return $ TypeInstanceDeclaration sa ch idx nm deps className [actualTy, inferredRepTy] (ExplicitInstance inst)
        | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys actualTy
      _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 2
  | otherwise = throwError . errorMessage' ss $ CannotDerive className tys
deriveInstance mn syns kinds ndis ds (TypeInstanceDeclaration sa@(ss, _) ch idx nm deps className tys NewtypeInstance) =
  case tys of
    _ : _ | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor (last tys)
          , mn == fromMaybe mn mn'
          -> TypeInstanceDeclaration sa ch idx nm deps className tys . NewtypeInstanceWithDictionary <$> deriveNewtypeInstance ss mn syns kinds ndis className ds tys tyCon args
          | otherwise -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys (last tys)
    _ -> throwError . errorMessage' ss $ InvalidNewtypeInstance className tys
deriveInstance _ _ _ _ _ e = return e

unwrapTypeConstructor :: SourceType -> Maybe (Qualified (ProperName 'TypeName), [SourceType])
unwrapTypeConstructor = fmap (second reverse) . go
  where
  go (TypeConstructor _ tyCon) = Just (tyCon, [])
  go (TypeApp _ ty arg) = do
    (tyCon, args) <- go ty
    return (tyCon, arg : args)
  go _ = Nothing

deriveNewtypeInstance
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => SourceSpan
  -> ModuleName
  -> SynonymMap
  -> KindMap
  -> NewtypeDerivedInstances
  -> Qualified (ProperName 'ClassName)
  -> [Declaration]
  -> [SourceType]
  -> ProperName 'TypeName
  -> [SourceType]
  -> m Expr
deriveNewtypeInstance ss mn syns kinds ndis className ds tys tyConNm dargs = do
    verifySuperclasses
    tyCon <- findTypeDecl ss tyConNm ds
    go tyCon
  where
    go (DataDeclaration _ Newtype _ tyArgNames [(DataConstructorDeclaration _ _ [(_, wrapped)])]) = do
      -- The newtype might not be applied to all type arguments.
      -- This is okay as long as the newtype wraps something which ends with
      -- sufficiently many type applications to variables.
      -- For example, we can derive Functor for
      --
      -- newtype MyArray a = MyArray (Array a)
      --
      -- since Array a is a type application which uses the last
      -- type argument
      wrapped' <- replaceAllTypeSynonymsM syns kinds wrapped
      case stripRight (takeReverse (length tyArgNames - length dargs) tyArgNames) wrapped' of
        Just wrapped'' -> do
          let subst = zipWith (\(name, _) t -> (name, t)) tyArgNames dargs
          wrapped''' <- replaceAllTypeSynonymsM syns kinds $ replaceAllTypeVars subst wrapped''
          tys' <- mapM (replaceAllTypeSynonymsM syns kinds) tys
          return (DeferredDictionary className (init tys' ++ [wrapped''']))
        Nothing -> throwError . errorMessage' ss $ InvalidNewtypeInstance className tys
    go _ = throwError . errorMessage' ss $ InvalidNewtypeInstance className tys

    takeReverse :: Int -> [a] -> [a]
    takeReverse n = take n . reverse

    stripRight :: [(Text, Maybe kind)] -> SourceType -> Maybe SourceType
    stripRight [] ty = Just ty
    stripRight ((arg, _) : args) (TypeApp _ t (TypeVar _ arg'))
      | arg == arg' = stripRight args t
    stripRight _ _ = Nothing

    verifySuperclasses :: m ()
    verifySuperclasses =
      for_ (M.lookup (qualify mn className) (ndiClasses ndis)) $ \(args, superclasses, _) ->
        for_ superclasses $ \Constraint{..} -> do
          let constraintClass' = qualify (error "verifySuperclasses: unknown class module") constraintClass
          for_ (M.lookup constraintClass' (ndiClasses ndis)) $ \(_, _, deps) ->
            -- We need to check whether the newtype is mentioned, because of classes like MonadWriter
            -- with its Monoid superclass constraint.
            when (not (null args) && any ((last args `elem`) . usedTypeVariables) constraintArgs) $ do
              -- For now, we only verify superclasses where the newtype is the only argument,
              -- or for which all other arguments are determined by functional dependencies.
              -- Everything else raises a UnverifiableSuperclassInstance warning.
              -- This covers pretty much all cases we're interested in, but later we might want to do
              -- more work to extend this to other superclass relationships.
              let determined = map (srcTypeVar . (args !!)) . ordNub . concatMap fdDetermined . filter ((== [length args - 1]) . fdDeterminers) $ deps
              if eqType (last constraintArgs) (srcTypeVar (last args)) && all (`elem` determined) (init constraintArgs)
                then do
                  -- Now make sure that a superclass instance was derived. Again, this is not a complete
                  -- check, since the superclass might have multiple type arguments, so overlaps might still
                  -- be possible, so we warn again.
                  for_ (extractNewtypeName mn tys) $ \nm ->
                    unless ((constraintClass', nm) `S.member` ndiDerivedInstances ndis) $
                      tell . errorMessage' ss $ MissingNewtypeSuperclassInstance constraintClass className tys
                else tell . errorMessage' ss $ UnverifiableSuperclassInstance constraintClass className tys

dataEq :: ModuleName
dataEq = ModuleName "Data.Eq"

dataOrd :: ModuleName
dataOrd = ModuleName "Data.Ord"

dataFunctor :: ModuleName
dataFunctor = ModuleName "Data.Functor"

unguarded :: Expr -> [GuardedExpr]
unguarded e = [MkUnguarded e]

deriveGenericRep
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => SourceSpan
  -> ModuleName
  -> SynonymMap
  -> KindMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> [SourceType]
  -> SourceType
  -> m ([Declaration], SourceType)
deriveGenericRep ss mn syns kinds ds tyConNm tyConArgs repTy = do
    checkIsWildcard ss tyConNm repTy
    go =<< findTypeDecl ss tyConNm ds
  where
    go :: Declaration -> m ([Declaration], SourceType)
    go (DataDeclaration (ss', _) _ _ args dctors) = do
      x <- freshIdent "x"
      (reps, to, from) <- unzip3 <$> traverse makeInst dctors
      let rep = toRepTy reps
          inst | null reps =
                   -- If there are no cases, spin
                   [ ValueDecl (ss', []) (Ident "to") Public [] $ unguarded $
                      lamCase ss' x
                        [ CaseAlternative
                            [NullBinder]
                            (unguarded (App (Var ss DataGenericRep.to) (Var ss' (Qualified Nothing x))))
                        ]
                   , ValueDecl (ss', []) (Ident "from") Public [] $ unguarded $
                      lamCase ss' x
                        [ CaseAlternative
                            [NullBinder]
                            (unguarded (App (Var ss DataGenericRep.from) (Var ss' (Qualified Nothing x))))
                        ]
                   ]
               | otherwise =
                   [ ValueDecl (ss', []) (Ident "to") Public [] $ unguarded $
                       lamCase ss' x (zipWith ($) (map underBinder (sumBinders (length dctors))) to)
                   , ValueDecl (ss', []) (Ident "from") Public [] $ unguarded $
                       lamCase ss' x (zipWith ($) (map underExpr (sumExprs (length dctors))) from)
                   ]

          subst = zipWith ((,) . fst) args tyConArgs
      return (inst, replaceAllTypeVars subst rep)
    go _ = internalError "deriveGenericRep go: expected DataDeclaration"

    select :: (a -> a) -> (a -> a) -> Int -> [a -> a]
    select _ _ 0 = []
    select _ _ 1 = [id]
    select l r n = take (n - 1) (iterate (r .) l) ++ [compN (n - 1) r]

    sumBinders :: Int -> [Binder -> Binder]
    sumBinders = select (ConstructorBinder ss DataGenericRep.Inl . pure)
                        (ConstructorBinder ss DataGenericRep.Inr . pure)

    sumExprs :: Int -> [Expr -> Expr]
    sumExprs = select (App (Constructor ss DataGenericRep.Inl))
                      (App (Constructor ss DataGenericRep.Inr))

    compN :: Int -> (a -> a) -> a -> a
    compN 0 _ = id
    compN n f = f . compN (n - 1) f

    makeInst
      :: DataConstructorDeclaration
      -> m (SourceType, CaseAlternative, CaseAlternative)
    makeInst (DataConstructorDeclaration _ ctorName args) = do
        args' <- mapM (replaceAllTypeSynonymsM syns kinds . snd) args
        (ctorTy, matchProduct, ctorArgs, matchCtor, mkProduct) <- makeProduct args'
        return ( srcTypeApp (srcTypeApp (srcTypeConstructor DataGenericRep.Constructor)
                                  (srcTypeLevelString $ mkString (runProperName ctorName)))
                         ctorTy
               , CaseAlternative [ ConstructorBinder ss DataGenericRep.Constructor [matchProduct] ]
                                 (unguarded (foldl' App (Constructor ss (Qualified (Just mn) ctorName)) ctorArgs))
               , CaseAlternative [ ConstructorBinder ss (Qualified (Just mn) ctorName) matchCtor ]
                                 (unguarded (App (Constructor ss DataGenericRep.Constructor) mkProduct))
               )

    makeProduct
      :: [SourceType]
      -> m (SourceType, Binder, [Expr], [Binder], Expr)
    makeProduct [] =
      pure (srcTypeConstructor DataGenericRep.NoArguments, NullBinder, [], [], Constructor ss DataGenericRep.NoArguments)
    makeProduct args = do
      (tys, bs1, es1, bs2, es2) <- unzip5 <$> traverse makeArg args
      pure ( foldr1 (\f -> srcTypeApp (srcTypeApp (srcTypeConstructor DataGenericRep.Product) f)) tys
           , foldr1 (\b1 b2 -> ConstructorBinder ss DataGenericRep.Product [b1, b2]) bs1
           , es1
           , bs2
           , foldr1 (\e1 -> App (App (Constructor ss DataGenericRep.Product) e1)) es2
           )

    makeArg :: SourceType -> m (SourceType, Binder, Expr, Binder, Expr)
    makeArg arg = do
      argName <- freshIdent "arg"
      pure ( srcTypeApp (srcTypeConstructor DataGenericRep.Argument) arg
           , ConstructorBinder ss DataGenericRep.Argument [ VarBinder ss argName ]
           , Var ss (Qualified Nothing argName)
           , VarBinder ss argName
           , App (Constructor ss DataGenericRep.Argument) (Var ss (Qualified Nothing argName))
           )

    underBinder :: (Binder -> Binder) -> CaseAlternative -> CaseAlternative
    underBinder f (CaseAlternative bs e) = CaseAlternative (map f bs) e

    underExpr :: (Expr -> Expr) -> CaseAlternative -> CaseAlternative
    underExpr f (CaseAlternative b [MkUnguarded e]) = CaseAlternative b (unguarded (f e))
    underExpr _ _ = internalError "underExpr: expected unguarded alternative"

    toRepTy :: [SourceType] -> SourceType
    toRepTy [] = srcTypeConstructor DataGenericRep.NoConstructors
    toRepTy [only] = only
    toRepTy ctors = foldr1 (\f -> srcTypeApp (srcTypeApp (srcTypeConstructor DataGenericRep.Sum) f)) ctors

checkIsWildcard :: MonadError MultipleErrors m => SourceSpan -> ProperName 'TypeName -> SourceType -> m ()
checkIsWildcard _ _ (TypeWildcard _ Nothing) = return ()
checkIsWildcard ss tyConNm _ =
  throwError . errorMessage' ss $ ExpectedWildcard tyConNm

deriveEq
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => SourceSpan
  -> ModuleName
  -> SynonymMap
  -> KindMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveEq ss mn syns kinds ds tyConNm = do
  tyCon <- findTypeDecl ss tyConNm ds
  eqFun <- mkEqFunction tyCon
  return [ ValueDecl (ss, []) (Ident Prelude.eq) Public [] (unguarded eqFun) ]
  where
    mkEqFunction :: Declaration -> m Expr
    mkEqFunction (DataDeclaration (ss', _) _ _ _ args) = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 ss' x y <$> (addCatch <$> mapM mkCtorClause args)
    mkEqFunction _ = internalError "mkEqFunction: expected DataDeclaration"

    preludeConj :: Expr -> Expr -> Expr
    preludeConj = App . App (Var ss (Qualified (Just (ModuleName "Data.HeytingAlgebra")) (Ident Prelude.conj)))

    preludeEq :: Expr -> Expr -> Expr
    preludeEq = App . App (Var ss (Qualified (Just dataEq) (Ident Prelude.eq)))

    preludeEq1 :: Expr -> Expr -> Expr
    preludeEq1 = App . App (Var ss (Qualified (Just dataEq) (Ident Prelude.eq1)))

    addCatch :: [CaseAlternative] -> [CaseAlternative]
    addCatch xs
      | length xs /= 1 = xs ++ [catchAll]
      | otherwise = xs -- Avoid redundant case
      where
      catchAll = CaseAlternative [NullBinder, NullBinder] (unguarded (Literal ss (BooleanLiteral False)))

    mkCtorClause :: DataConstructorDeclaration -> m CaseAlternative
    mkCtorClause (DataConstructorDeclaration _ ctorName tys) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM (replaceAllTypeSynonymsM syns kinds . snd) tys
      let tests = zipWith3 toEqTest (map (Var ss . Qualified Nothing) identsL) (map (Var ss . Qualified Nothing) identsR) tys'
      return $ CaseAlternative [caseBinder identsL, caseBinder identsR] (unguarded (conjAll tests))
      where
      caseBinder idents = ConstructorBinder ss (Qualified (Just mn) ctorName) (map (VarBinder ss) idents)

    conjAll :: [Expr] -> Expr
    conjAll [] = Literal ss (BooleanLiteral True)
    conjAll xs = foldl1 preludeConj xs

    toEqTest :: Expr -> Expr -> SourceType -> Expr
    toEqTest l r ty
      | Just rec <- objectType ty
      , Just fields <- decomposeRec rec =
          conjAll
          . map (\((Label str), typ) -> toEqTest (Accessor str l) (Accessor str r) typ)
          $ fields
      | isAppliedVar ty = preludeEq1 l r
      | otherwise = preludeEq l r

deriveEq1 :: SourceSpan -> [Declaration]
deriveEq1 ss =
  [ ValueDecl (ss, []) (Ident Prelude.eq1) Public [] (unguarded preludeEq)]
  where
    preludeEq :: Expr
    preludeEq = Var ss (Qualified (Just dataEq) (Ident Prelude.eq))

deriveOrd
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => SourceSpan
  -> ModuleName
  -> SynonymMap
  -> KindMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveOrd ss mn syns kinds ds tyConNm = do
  tyCon <- findTypeDecl ss tyConNm ds
  compareFun <- mkCompareFunction tyCon
  return [ ValueDecl (ss, []) (Ident Prelude.compare) Public [] (unguarded compareFun) ]
  where
    mkCompareFunction :: Declaration -> m Expr
    mkCompareFunction (DataDeclaration (ss', _) _ _ _ args) = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 ss' x y <$> (addCatch . concat <$> mapM mkCtorClauses (splitLast args))
    mkCompareFunction _ = internalError "mkCompareFunction: expected DataDeclaration"

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

    orderingName :: Text -> Qualified (ProperName a)
    orderingName = Qualified (Just (ModuleName "Data.Ordering")) . ProperName

    orderingCtor :: Text -> Expr
    orderingCtor = Constructor ss . orderingName

    orderingBinder :: Text -> Binder
    orderingBinder name = ConstructorBinder ss (orderingName name) []

    ordCompare :: Expr -> Expr -> Expr
    ordCompare = App . App (Var ss (Qualified (Just dataOrd) (Ident Prelude.compare)))

    ordCompare1 :: Expr -> Expr -> Expr
    ordCompare1 = App . App (Var ss (Qualified (Just dataOrd) (Ident Prelude.compare1)))

    mkCtorClauses :: (DataConstructorDeclaration, Bool) -> m [CaseAlternative]
    mkCtorClauses ((DataConstructorDeclaration _ ctorName tys), isLast) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM (replaceAllTypeSynonymsM syns kinds . snd) tys
      let tests = zipWith3 toOrdering (map (Var ss . Qualified Nothing) identsL) (map (Var ss . Qualified Nothing) identsR) tys'
          extras | not isLast = [ CaseAlternative [ ConstructorBinder ss (Qualified (Just mn) ctorName) (replicate (length tys) NullBinder)
                                                  , NullBinder
                                                  ]
                                                  (unguarded (orderingCtor "LT"))
                                , CaseAlternative [ NullBinder
                                                  , ConstructorBinder ss (Qualified (Just mn) ctorName) (replicate (length tys) NullBinder)
                                                  ]
                                                  (unguarded (orderingCtor "GT"))
                                ]
                 | otherwise = []
      return $ CaseAlternative [ caseBinder identsL
                               , caseBinder identsR
                               ]
                               (unguarded (appendAll tests))
             : extras

      where
      caseBinder idents = ConstructorBinder ss (Qualified (Just mn) ctorName) (map (VarBinder ss) idents)

    appendAll :: [Expr] -> Expr
    appendAll [] = orderingCtor "EQ"
    appendAll [x] = x
    appendAll (x : xs) = Case [x] [ CaseAlternative [orderingBinder "LT"]
                                                    (unguarded (orderingCtor "LT"))
                                  , CaseAlternative [orderingBinder "GT"]
                                                    (unguarded (orderingCtor "GT"))
                                  , CaseAlternative [ NullBinder ]
                                                    (unguarded (appendAll xs))
                                  ]

    toOrdering :: Expr -> Expr -> SourceType -> Expr
    toOrdering l r ty
      | Just rec <- objectType ty
      , Just fields <- decomposeRec rec =
          appendAll
          . map (\((Label str), typ) -> toOrdering (Accessor str l) (Accessor str r) typ)
          $ fields
      | isAppliedVar ty = ordCompare1 l r
      | otherwise = ordCompare l r

deriveOrd1 :: SourceSpan -> [Declaration]
deriveOrd1 ss =
  [ ValueDecl (ss, []) (Ident Prelude.compare1) Public [] (unguarded dataOrdCompare)]
  where
    dataOrdCompare :: Expr
    dataOrdCompare = Var ss (Qualified (Just dataOrd) (Ident Prelude.compare))

deriveNewtype
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => SourceSpan
  -> SynonymMap
  -> KindMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> [SourceType]
  -> SourceType
  -> m SourceType
deriveNewtype ss syns kinds ds tyConNm tyConArgs unwrappedTy = do
    checkIsWildcard ss tyConNm unwrappedTy
    go =<< findTypeDecl ss tyConNm ds
  where
    go :: Declaration -> m SourceType
    go (DataDeclaration (ss', _) Data name _ _) =
      throwError . errorMessage' ss' $ CannotDeriveNewtypeForData name
    go (DataDeclaration _ Newtype name args dctors) = do
      checkNewtype name dctors
      let (DataConstructorDeclaration _ _ [(_, ty)]) = head dctors
      ty' <- replaceAllTypeSynonymsM syns kinds ty
      let subst = zipWith ((,) . fst) args tyConArgs
      return $ replaceAllTypeVars subst ty'
    go _ = internalError "deriveNewtype go: expected DataDeclaration"

findTypeDecl
  :: (MonadError MultipleErrors m)
  => SourceSpan
  -> ProperName 'TypeName
  -> [Declaration]
  -> m Declaration
findTypeDecl ss tyConNm = maybe (throwError . errorMessage' ss $ CannotFindDerivingType tyConNm) return . find isTypeDecl
  where
  isTypeDecl :: Declaration -> Bool
  isTypeDecl (DataDeclaration _ _ nm _ _) | nm == tyConNm = True
  isTypeDecl _ = False

lam :: SourceSpan -> Ident -> Expr -> Expr
lam ss = Abs . VarBinder ss

lamCase :: SourceSpan -> Ident -> [CaseAlternative] -> Expr
lamCase ss s = lam ss s . Case [mkVar ss s]

lamCase2 :: SourceSpan -> Ident -> Ident -> [CaseAlternative] -> Expr
lamCase2 ss s t = lam ss s . lam ss t . Case [mkVar ss s, mkVar ss t]

mkVarMn :: SourceSpan -> Maybe ModuleName -> Ident -> Expr
mkVarMn ss mn = Var ss . Qualified mn

mkVar :: SourceSpan -> Ident -> Expr
mkVar ss = mkVarMn ss Nothing

isAppliedVar :: Type a -> Bool
isAppliedVar (TypeApp _ (TypeVar _ _) _) = True
isAppliedVar _ = False

objectType :: Type a -> Maybe (Type a)
objectType (TypeApp _ (TypeConstructor _ Prim.Record) rec) = Just rec
objectType _ = Nothing

decomposeRec :: SourceType -> Maybe [(Label, SourceType)]
decomposeRec = fmap (sortBy (comparing fst)) . go
  where go (RCons _ str typ typs) = fmap ((str, typ) :) (go typs)
        go (REmptyKinded _ _) = Just []
        go _ = Nothing

decomposeRec' :: SourceType -> [(Label, SourceType)]
decomposeRec' = sortBy (comparing fst) . go
  where go (RCons _ str typ typs) = (str, typ) : go typs
        go _ = []

deriveFunctor
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => SourceSpan
  -> ModuleName
  -> SynonymMap
  -> KindMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveFunctor ss mn syns kinds ds tyConNm = do
  tyCon <- findTypeDecl ss tyConNm ds
  mapFun <- mkMapFunction tyCon
  return [ ValueDecl (ss, []) (Ident Prelude.map) Public [] (unguarded mapFun) ]
  where
    mkMapFunction :: Declaration -> m Expr
    mkMapFunction (DataDeclaration (ss', _) _ _ tys ctors) = case reverse tys of
      [] -> throwError . errorMessage' ss' $ KindsDoNotUnify (kindType -:> kindType) kindType
      ((iTy, _) : _) -> do
        f <- freshIdent "f"
        m <- freshIdent "m"
        lam ss' f . lamCase ss' m <$> mapM (mkCtorClause iTy f) ctors
    mkMapFunction _ = internalError "mkMapFunction: expected DataDeclaration"

    mkCtorClause :: Text -> Ident -> DataConstructorDeclaration -> m CaseAlternative
    mkCtorClause iTyName f (DataConstructorDeclaration _ ctorName ctorTys) = do
      idents <- replicateM (length ctorTys) (freshIdent "v")
      ctorTys' <- mapM (replaceAllTypeSynonymsM syns kinds . snd) ctorTys
      args <- zipWithM transformArg idents ctorTys'
      let ctor = Constructor ss (Qualified (Just mn) ctorName)
          rebuilt = foldl' App ctor args
          caseBinder = ConstructorBinder ss (Qualified (Just mn) ctorName) (VarBinder ss <$> idents)
      return $ CaseAlternative [caseBinder] (unguarded rebuilt)
      where
        fVar = mkVar ss f
        mapVar = mkVarMn ss (Just dataFunctor) (Ident Prelude.map)

        -- TODO: deal with type synonyms, ala https://github.com/purescript/purescript/pull/2516
        transformArg :: Ident -> SourceType -> m Expr
        transformArg ident = fmap (foldr App (mkVar ss ident)) . goType where

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
                let argVar = mkVar ss arg
                    mkAssignment ((Label l), x) = (l, App x (Accessor l argVar))
                return (lam ss arg (ObjectUpdate argVar (mkAssignment <$> updates)))

          -- quantifiers
          goType (ForAll _ scopedVar _ t _) | scopedVar /= iTyName = goType t

          -- constraints
          goType (ConstrainedType _ _ t) = goType t

          -- under a `* -> *`, just assume functor for now
          goType (TypeApp _ _ t) = fmap (App mapVar) <$> goType t

          -- otherwise do nothing - will fail type checking if type does actually contain index
          goType _ = return Nothing
