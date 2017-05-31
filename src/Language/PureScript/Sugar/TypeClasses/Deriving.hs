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
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.PureScript.AST
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Kinds
import           Language.PureScript.Names
import           Language.PureScript.Label (Label(..))
import           Language.PureScript.PSString (mkString, decodeStringEither)
import           Language.PureScript.Types
import           Language.PureScript.TypeChecker (checkNewtype)
import           Language.PureScript.TypeChecker.Synonyms (SynonymMap, replaceAllTypeSynonymsM)

-- | When deriving an instance for a newtype, we must ensure that all superclass
-- instances were derived in the same way. This data structure is used to ensure
-- this property.
data NewtypeDerivedInstances = NewtypeDerivedInstances
  { ndiClasses :: M.Map (ModuleName, ProperName 'ClassName) ([Text], [Constraint], [FunctionalDependency])
  -- ^ A list of superclass constraints for each type class. Since type classes
  -- have not been desugared here, we need to track this.
  , ndiDerivedInstances :: S.Set ((ModuleName, ProperName 'ClassName), (ModuleName, ProperName 'TypeName))
  -- ^ A list of newtype instances which were derived in this module.
  } deriving Show

instance Monoid NewtypeDerivedInstances where
  mempty = NewtypeDerivedInstances mempty mempty
  mappend x y =
    NewtypeDerivedInstances { ndiClasses          = ndiClasses          x <> ndiClasses          y
                            , ndiDerivedInstances = ndiDerivedInstances x <> ndiDerivedInstances y
                            }

-- | Extract the name of the newtype appearing in the last type argument of
-- a derived newtype instance.
--
-- Note: since newtypes in newtype instances can only be applied to type arguments
-- (no flexible instances allowed), we don't need to bother with unification when
-- looking for matching superclass instances, which saves us a lot of work. Instead,
-- we just match the newtype name.
extractNewtypeName :: ModuleName -> [Type] -> Maybe (ModuleName, ProperName 'TypeName)
extractNewtypeName _ [] = Nothing
extractNewtypeName mn xs = go (last xs) where
  go (TypeApp ty (TypeVar _)) = go ty
  go (TypeConstructor name) = Just (qualify mn name)
  go _ = Nothing

-- | Elaborates deriving instance declarations by code generation.
deriveInstances
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadSupply m)
  => [ExternsFile]
  -> Module
  -> m Module
deriveInstances externs (Module ss coms mn ds exts) =
    Module ss coms mn <$> mapM (deriveInstance mn synonyms instanceData ds) ds <*> pure exts
  where
    -- We need to collect type synonym information, since synonyms will not be
    -- removed until later, during type checking.
    synonyms :: SynonymMap
    synonyms =
        M.fromList $ (externs >>= \ExternsFile{..} -> mapMaybe (fromExternsDecl efModuleName) efDeclarations)
                  ++ mapMaybe fromLocalDecl ds
      where
        fromExternsDecl mn' (EDTypeSynonym name args ty) = Just (Qualified (Just mn') name, (args, ty))
        fromExternsDecl _ _ = Nothing

        fromLocalDecl (TypeSynonymDeclaration name args ty) = do
          Just (Qualified (Just mn) name, (args, ty))
        fromLocalDecl (PositionedDeclaration _ _ d) = fromLocalDecl d
        fromLocalDecl _ = Nothing

    instanceData :: NewtypeDerivedInstances
    instanceData =
        foldMap (\ExternsFile{..} -> foldMap (fromExternsDecl efModuleName) efDeclarations) externs <> foldMap fromLocalDecl ds
      where
        fromExternsDecl mn' EDClass{..} =
          NewtypeDerivedInstances (M.singleton (mn', edClassName) (map fst edClassTypeArguments, edClassConstraints, edFunctionalDependencies)) mempty
        fromExternsDecl mn' EDInstance{..} =
          foldMap (\nm -> NewtypeDerivedInstances mempty (S.singleton (qualify mn' edInstanceClassName, nm))) (extractNewtypeName mn' edInstanceTypes)
        fromExternsDecl _ _ = mempty

        fromLocalDecl (TypeClassDeclaration cl args cons deps _) =
          NewtypeDerivedInstances (M.singleton (mn, cl) (map fst args, cons, deps)) mempty
        fromLocalDecl (TypeInstanceDeclaration _ _ cl tys _) =
          foldMap (\nm -> NewtypeDerivedInstances mempty (S.singleton (qualify mn cl, nm))) (extractNewtypeName mn tys)
        fromLocalDecl (PositionedDeclaration _ _ d) = fromLocalDecl d
        fromLocalDecl _ = mempty

-- | Takes a declaration, and if the declaration is a deriving TypeInstanceDeclaration,
-- elaborates that into an instance declaration via code generation.
deriveInstance
  :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> NewtypeDerivedInstances
  -> [Declaration]
  -> Declaration
  -> m Declaration
deriveInstance mn syns _ ds (TypeInstanceDeclaration nm deps className tys DerivedInstance)
  | className == Qualified (Just dataGeneric) (ProperName C.generic)
  = case tys of
      [ty] | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveGeneric mn syns ds tyCon args
           | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataEq) (ProperName "Eq")
  = case tys of
      [ty] | Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveEq mn syns ds tyCon
           | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataOrd) (ProperName "Ord")
  = case tys of
      [ty] | Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveOrd mn syns ds tyCon
           | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataFunctor) (ProperName "Functor")
  = case tys of
      [ty] | Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
           , mn == fromMaybe mn mn'
           -> TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveFunctor mn syns ds tyCon
           | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys ty
      _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 1
  | className == Qualified (Just dataNewtype) (ProperName "Newtype")
  = case tys of
      [wrappedTy, unwrappedTy]
        | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor wrappedTy
        , mn == fromMaybe mn mn'
        -> do (inst, actualUnwrappedTy) <- deriveNewtype mn syns ds tyCon args unwrappedTy
              return $ TypeInstanceDeclaration nm deps className [wrappedTy, actualUnwrappedTy] (ExplicitInstance inst)
        | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys wrappedTy
      _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 2
  | className == Qualified (Just dataGenericRep) (ProperName C.generic)
  = case tys of
      [actualTy, repTy]
        | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor actualTy
        , mn == fromMaybe mn mn'
        -> do (inst, inferredRepTy) <- deriveGenericRep mn syns ds tyCon args repTy
              return $ TypeInstanceDeclaration nm deps className [actualTy, inferredRepTy] (ExplicitInstance inst)
        | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys actualTy
      _ -> throwError . errorMessage $ InvalidDerivedInstance className tys 2
  | otherwise = throwError . errorMessage $ CannotDerive className tys
deriveInstance mn syns ndis ds (TypeInstanceDeclaration nm deps className tys NewtypeInstance) =
  case tys of
    _ : _ | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor (last tys)
          , mn == fromMaybe mn mn'
          -> TypeInstanceDeclaration nm deps className tys . NewtypeInstanceWithDictionary <$> deriveNewtypeInstance mn syns ndis className ds tys tyCon args
          | otherwise -> throwError . errorMessage $ ExpectedTypeConstructor className tys (last tys)
    _ -> throwError . errorMessage $ InvalidNewtypeInstance className tys
deriveInstance mn syns ndis ds (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> deriveInstance mn syns ndis ds d
deriveInstance _ _ _ _ e = return e

unwrapTypeConstructor :: Type -> Maybe (Qualified (ProperName 'TypeName), [Type])
unwrapTypeConstructor = fmap (second reverse) . go
  where
  go (TypeConstructor tyCon) = Just (tyCon, [])
  go (TypeApp ty arg) = do
    (tyCon, args) <- go ty
    return (tyCon, arg : args)
  go _ = Nothing

deriveNewtypeInstance
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> SynonymMap
  -> NewtypeDerivedInstances
  -> Qualified (ProperName 'ClassName)
  -> [Declaration]
  -> [Type]
  -> ProperName 'TypeName
  -> [Type]
  -> m Expr
deriveNewtypeInstance mn syns ndis className ds tys tyConNm dargs = do
    verifySuperclasses
    tyCon <- findTypeDecl tyConNm ds
    go tyCon
  where
    go (DataDeclaration Newtype _ tyArgNames [(_, [wrapped])])
      -- The newtype might not be applied to all type arguments.
      -- This is okay as long as the newtype wraps something which ends with
      -- sufficiently many type applications to variables.
      -- For example, we can derive Functor for
      --
      -- newtype MyArray a = MyArray (Array a)
      --
      -- since Array a is a type application which uses the last
      -- type argument
      | Just wrapped' <- stripRight (takeReverse (length tyArgNames - length dargs) tyArgNames) wrapped =
          do let subst = zipWith (\(name, _) t -> (name, t)) tyArgNames dargs
             wrapped'' <- replaceAllTypeSynonymsM syns wrapped'
             return (DeferredDictionary className (init tys ++ [replaceAllTypeVars subst wrapped'']))
    go (PositionedDeclaration _ _ d) = go d
    go _ = throwError . errorMessage $ InvalidNewtypeInstance className tys

    takeReverse :: Int -> [a] -> [a]
    takeReverse n = take n . reverse

    stripRight :: [(Text, Maybe kind)] -> Type -> Maybe Type
    stripRight [] ty = Just ty
    stripRight ((arg, _) : args) (TypeApp t (TypeVar arg'))
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
              let determined = map (TypeVar . (args !!)) . ordNub . concatMap fdDetermined . filter ((== [length args - 1]) . fdDeterminers) $ deps
              if last constraintArgs == TypeVar (last args) && all (`elem` determined) (init constraintArgs)
                then do
                  -- Now make sure that a superclass instance was derived. Again, this is not a complete
                  -- check, since the superclass might have multiple type arguments, so overlaps might still
                  -- be possible, so we warn again.
                  for_ (extractNewtypeName mn tys) $ \nm ->
                    unless ((constraintClass', nm) `S.member` ndiDerivedInstances ndis) $
                      tell . errorMessage $ MissingNewtypeSuperclassInstance constraintClass className tys
                else tell . errorMessage $ UnverifiableSuperclassInstance constraintClass className tys

dataGeneric :: ModuleName
dataGeneric = ModuleName [ ProperName "Data", ProperName "Generic" ]

dataGenericRep :: ModuleName
dataGenericRep = ModuleName [ ProperName "Data", ProperName "Generic", ProperName "Rep" ]

dataMaybe :: ModuleName
dataMaybe = ModuleName [ ProperName "Data", ProperName "Maybe" ]

typesProxy :: ModuleName
typesProxy = ModuleName [ ProperName "Type", ProperName "Proxy" ]

dataEq :: ModuleName
dataEq = ModuleName [ ProperName "Data", ProperName "Eq" ]

dataOrd :: ModuleName
dataOrd = ModuleName [ ProperName "Data", ProperName "Ord" ]

dataNewtype :: ModuleName
dataNewtype = ModuleName [ ProperName "Data", ProperName "Newtype" ]

dataFunctor :: ModuleName
dataFunctor = ModuleName [ ProperName "Data", ProperName "Functor" ]

unguarded :: Expr -> [GuardedExpr]
unguarded e = [MkUnguarded e]

deriveGeneric
  :: forall m. (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> m [Declaration]
deriveGeneric mn syns ds tyConNm dargs = do
  tyCon <- findTypeDecl tyConNm ds
  toSpine <- mkSpineFunction tyCon
  fromSpine <- mkFromSpineFunction tyCon
  toSignature <- mkSignatureFunction tyCon dargs
  return [ ValueDeclaration (Ident C.toSpine) Public [] (unguarded toSpine)
         , ValueDeclaration (Ident C.fromSpine) Public [] (unguarded fromSpine)
         , ValueDeclaration (Ident C.toSignature) Public [] (unguarded toSignature)
         ]
  where
    mkSpineFunction :: Declaration -> m Expr
    mkSpineFunction (DataDeclaration _ _ _ args) = do
      x <- freshIdent'
      lamCase x <$> mapM mkCtorClause args
      where
      prodConstructor :: Expr -> Expr
      prodConstructor = App (Constructor (Qualified (Just dataGeneric) (ProperName "SProd")))

      recordConstructor :: Expr -> Expr
      recordConstructor = App (Constructor (Qualified (Just dataGeneric) (ProperName "SRecord")))

      mkCtorClause :: (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
      mkCtorClause (ctorName, tys) = do
        idents <- replicateM (length tys) freshIdent'
        tys' <- mapM (replaceAllTypeSynonymsM syns) tys
        let caseResult =
              App (prodConstructor (Literal . StringLiteral . mkString . showQualified runProperName $ Qualified (Just mn) ctorName))
                . Literal . ArrayLiteral
                $ zipWith toSpineFun (map (Var . Qualified Nothing) idents) tys'
        return $ CaseAlternative [ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)] (unguarded caseResult)

      toSpineFun :: Expr -> Type -> Expr
      toSpineFun i r | Just rec <- objectType r
                     , Just fields <- decomposeRec rec =
        lamNull . recordConstructor . Literal . ArrayLiteral
          . map
            (\((Label str),typ) ->
              Literal $ ObjectLiteral
                [ ("recLabel", Literal (StringLiteral str))
                , ("recValue", toSpineFun (Accessor str i) typ)
                ]
            )
          $ fields
      toSpineFun i _ = lamNull $ App (mkGenVar (Ident C.toSpine)) i
    mkSpineFunction (PositionedDeclaration _ _ d) = mkSpineFunction d
    mkSpineFunction _ = internalError "mkSpineFunction: expected DataDeclaration"

    mkSignatureFunction :: Declaration -> [Type] -> m Expr
    mkSignatureFunction (DataDeclaration _ name tyArgs args) classArgs = lamNull . mkSigProd <$> mapM mkProdClause args
      where
      mkSigProd :: [Expr] -> Expr
      mkSigProd =
        App
          (App
            (Constructor (Qualified (Just dataGeneric) (ProperName "SigProd")))
            (Literal (StringLiteral $ mkString (showQualified runProperName (Qualified (Just mn) name))))
          )
          . Literal
          . ArrayLiteral

      mkSigRec :: [Expr] -> Expr
      mkSigRec = App (Constructor (Qualified (Just dataGeneric) (ProperName "SigRecord"))) . Literal . ArrayLiteral

      proxy :: Type -> Type
      proxy = TypeApp (TypeConstructor (Qualified (Just typesProxy) (ProperName "Proxy")))

      mkProdClause :: (ProperName 'ConstructorName, [Type]) -> m Expr
      mkProdClause (ctorName, tys) = do
        tys' <- mapM (replaceAllTypeSynonymsM syns) tys
        return $ Literal $ ObjectLiteral
          [ ("sigConstructor", Literal (StringLiteral $ mkString (showQualified runProperName (Qualified (Just mn) ctorName))))
          , ("sigValues", Literal . ArrayLiteral . map (mkProductSignature . instantiate) $ tys')
          ]

      mkProductSignature :: Type -> Expr
      mkProductSignature r | Just rec <- objectType r
                           , Just fields <- decomposeRec rec =
          lamNull . mkSigRec $
            [ Literal $ ObjectLiteral
                [ ("recLabel", Literal (StringLiteral str))
                , ("recValue", mkProductSignature typ)
                ]
            | ((Label str), typ) <- fields
            ]
      mkProductSignature typ = lamNull $ App (mkGenVar (Ident C.toSignature))
                               (TypedValue False (mkGenVar (Ident "anyProxy")) (proxy typ))
      instantiate = replaceAllTypeVars (zipWith (\(arg, _) ty -> (arg, ty)) tyArgs classArgs)
    mkSignatureFunction (PositionedDeclaration _ _ d) classArgs = mkSignatureFunction d classArgs
    mkSignatureFunction _ _ = internalError "mkSignatureFunction: expected DataDeclaration"

    mkFromSpineFunction :: Declaration -> m Expr
    mkFromSpineFunction (DataDeclaration _ _ _ args) = do
      x <- freshIdent'
      lamCase x <$> (addCatch <$> mapM mkAlternative args)
      where
      mkJust :: Expr -> Expr
      mkJust = App (Constructor (Qualified (Just dataMaybe) (ProperName "Just")))

      mkNothing :: Expr
      mkNothing = Constructor (Qualified (Just dataMaybe) (ProperName "Nothing"))

      prodBinder :: [Binder] -> Binder
      prodBinder = ConstructorBinder (Qualified (Just dataGeneric) (ProperName "SProd"))

      recordBinder :: [Binder] -> Binder
      recordBinder = ConstructorBinder (Qualified (Just dataGeneric) (ProperName "SRecord"))

      mkAlternative :: (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
      mkAlternative (ctorName, tys) = do
        idents <- replicateM (length tys) freshIdent'
        tys' <- mapM (replaceAllTypeSynonymsM syns) tys
        return $
          CaseAlternative
            [ prodBinder
                [ LiteralBinder (StringLiteral $ mkString (showQualified runProperName (Qualified (Just mn) ctorName)))
                , LiteralBinder (ArrayLiteral (map VarBinder idents))
                ]
            ]
            . unguarded
            $ liftApplicative
                (mkJust $ Constructor (Qualified (Just mn) ctorName))
                (zipWith fromSpineFun (map (Var . Qualified Nothing) idents) tys')

      addCatch :: [CaseAlternative] -> [CaseAlternative]
      addCatch = (++ [catchAll])
        where
        catchAll = CaseAlternative [NullBinder] (unguarded mkNothing)

      fromSpineFun :: Expr -> Type -> Expr
      fromSpineFun e r
        | Just rec <- objectType r
        , Just fields <- decomposeRec rec
        = App (lamCase (Ident "r") [ mkRecCase fields
                                   , CaseAlternative [NullBinder] (unguarded mkNothing)
                                   ])
              (App e unitVal)
      fromSpineFun e _ = App (mkGenVar (Ident C.fromSpine)) (App e unitVal)

      mkRecCase :: [(Label, Type)] -> CaseAlternative
      mkRecCase rs =
        CaseAlternative
          [ recordBinder [ LiteralBinder (ArrayLiteral (map (VarBinder . labelToIdent . fst) rs)) ] ]
          . unguarded
          $ liftApplicative (mkRecFun rs) (map (\(x, y) -> fromSpineFun (Accessor "recValue" (mkVar $ labelToIdent x)) y) rs)

      mkRecFun :: [(Label, Type)] -> Expr
      mkRecFun xs = mkJust $ foldr (lam . labelToIdent . fst) recLiteral xs
        where recLiteral = Literal . ObjectLiteral $ map (\(l@(Label s), _) -> (s, mkVar $ labelToIdent l)) xs
    mkFromSpineFunction (PositionedDeclaration _ _ d) = mkFromSpineFunction d
    mkFromSpineFunction _ = internalError "mkFromSpineFunction: expected DataDeclaration"

    -- Helpers

    liftApplicative :: Expr -> [Expr] -> Expr
    liftApplicative = foldl' (\x e -> App (App applyFn x) e)

    unitVal :: Expr
    unitVal = mkVarMn (Just (ModuleName [ProperName "Data", ProperName "Unit"])) (Ident "unit")

    applyFn :: Expr
    applyFn = mkVarMn (Just (ModuleName [ProperName "Control", ProperName "Apply"])) (Ident "apply")

    mkGenVar :: Ident -> Expr
    mkGenVar = mkVarMn (Just (ModuleName [ProperName "Data", ProperName C.generic]))

deriveGenericRep
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> Type
  -> m ([Declaration], Type)
deriveGenericRep mn syns ds tyConNm tyConArgs repTy = do
    checkIsWildcard tyConNm repTy
    go =<< findTypeDecl tyConNm ds
  where
    go :: Declaration -> m ([Declaration], Type)
    go (DataDeclaration _ _ args dctors) = do
      x <- freshIdent "x"
      (reps, to, from) <- unzip3 <$> traverse makeInst dctors
      let rep = toRepTy reps
          inst | null reps =
                   -- If there are no cases, spin
                   [ ValueDeclaration (Ident "to") Public [] $ unguarded $
                       lamCase x [ CaseAlternative [NullBinder]
                                                   (unguarded (App toName (Var (Qualified Nothing x))))
                                 ]
                   , ValueDeclaration (Ident "from") Public [] $ unguarded $
                       lamCase x [ CaseAlternative [NullBinder]
                                                   (unguarded (App fromName (Var (Qualified Nothing x))))
                                 ]
                   ]
               | otherwise =
                   [ ValueDeclaration (Ident "to") Public [] $ unguarded $
                       lamCase x (zipWith ($) (map underBinder (sumBinders (length dctors))) to)
                   , ValueDeclaration (Ident "from") Public [] $ unguarded $
                       lamCase x (zipWith ($) (map underExpr (sumExprs (length dctors))) from)
                   ]

          subst = zipWith ((,) . fst) args tyConArgs
      return (inst, replaceAllTypeVars subst rep)
    go (PositionedDeclaration _ _ d) = go d
    go _ = internalError "deriveGenericRep go: expected DataDeclaration"

    select :: (a -> a) -> (a -> a) -> Int -> [a -> a]
    select _ _ 0 = []
    select _ _ 1 = [id]
    select l r n = take (n - 1) (iterate (r .) l) ++ [compN (n - 1) r]

    sumBinders :: Int -> [Binder -> Binder]
    sumBinders = select (ConstructorBinder inl . pure) (ConstructorBinder inr . pure)

    sumExprs :: Int -> [Expr -> Expr]
    sumExprs = select (App (Constructor inl)) (App (Constructor inr))

    compN :: Int -> (a -> a) -> a -> a
    compN 0 _ = id
    compN n f = f . compN (n - 1) f

    makeInst
      :: (ProperName 'ConstructorName, [Type])
      -> m (Type, CaseAlternative, CaseAlternative)
    makeInst (ctorName, args) = do
        args' <- mapM (replaceAllTypeSynonymsM syns) args
        (ctorTy, matchProduct, ctorArgs, matchCtor, mkProduct) <- makeProduct args'
        return ( TypeApp (TypeApp (TypeConstructor constructor)
                                  (TypeLevelString $ mkString (runProperName ctorName)))
                         ctorTy
               , CaseAlternative [ ConstructorBinder constructor [matchProduct] ]
                                 (unguarded (foldl App (Constructor (Qualified (Just mn) ctorName)) ctorArgs))
               , CaseAlternative [ ConstructorBinder (Qualified (Just mn) ctorName) matchCtor ]
                                 (unguarded (constructor' mkProduct))
               )

    makeProduct
      :: [Type]
      -> m (Type, Binder, [Expr], [Binder], Expr)
    makeProduct [] =
      pure (noArgs, NullBinder, [], [], noArgs')
    makeProduct args = do
      (tys, bs1, es1, bs2, es2) <- unzip5 <$> traverse makeArg args
      pure ( foldr1 (\f -> TypeApp (TypeApp (TypeConstructor productName) f)) tys
           , foldr1 (\b1 b2 -> ConstructorBinder productName [b1, b2]) bs1
           , es1
           , bs2
           , foldr1 (\e1 -> App (App (Constructor productName) e1)) es2
           )

    makeArg :: Type -> m (Type, Binder, Expr, Binder, Expr)
    makeArg arg | Just rec <- objectType arg
                , Just fields <- decomposeRec rec = do
      fieldNames <- traverse freshIdent (map (runIdent . labelToIdent . fst) fields)
      case fieldNames of
        [] ->
          pure ( TypeApp (TypeConstructor record) noArgs
               , ConstructorBinder record [ NullBinder ]
               , Literal (ObjectLiteral [])
               , LiteralBinder (ObjectLiteral [])
               , record' noArgs'
               )
        _ ->
          pure ( TypeApp (TypeConstructor record)
                   (foldr1 (\f -> TypeApp (TypeApp (TypeConstructor productName) f))
                     (map (\((Label name), ty) ->
                       TypeApp (TypeApp (TypeConstructor field) (TypeLevelString name)) ty) fields))
               , ConstructorBinder record
                   [ foldr1 (\b1 b2 -> ConstructorBinder productName [b1, b2])
                       (map (\ident -> ConstructorBinder field [VarBinder ident]) fieldNames)
                   ]
               , Literal . ObjectLiteral $
                    zipWith (\((Label name), _) ident -> (name, Var (Qualified Nothing ident))) fields fieldNames
               , LiteralBinder . ObjectLiteral $
                     zipWith (\((Label name), _) ident -> (name, VarBinder ident)) fields fieldNames
               , record' $
                   foldr1 (\e1 -> App (App (Constructor productName) e1))
                     (map (field' . Var . Qualified Nothing) fieldNames)
               )
    makeArg arg = do
      argName <- freshIdent "arg"
      pure ( TypeApp (TypeConstructor argument) arg
           , ConstructorBinder argument [ VarBinder argName ]
           , Var (Qualified Nothing argName)
           , VarBinder argName
           , argument' (Var (Qualified Nothing argName))
           )

    underBinder :: (Binder -> Binder) -> CaseAlternative -> CaseAlternative
    underBinder f (CaseAlternative bs e) = CaseAlternative (map f bs) e

    underExpr :: (Expr -> Expr) -> CaseAlternative -> CaseAlternative
    underExpr f (CaseAlternative b [MkUnguarded e]) = CaseAlternative b (unguarded (f e))
    underExpr _ _ = internalError "underExpr: expected unguarded alternative"

    toRepTy :: [Type] -> Type
    toRepTy [] = noCtors
    toRepTy [only] = only
    toRepTy ctors = foldr1 (\f -> TypeApp (TypeApp sumCtor f)) ctors

    toName :: Expr
    toName = Var (Qualified (Just dataGenericRep) (Ident "to"))

    fromName :: Expr
    fromName = Var (Qualified (Just dataGenericRep) (Ident "from"))

    noCtors :: Type
    noCtors = TypeConstructor (Qualified (Just dataGenericRep) (ProperName "NoConstructors"))

    noArgs :: Type
    noArgs = TypeConstructor (Qualified (Just dataGenericRep) (ProperName "NoArguments"))

    noArgs' :: Expr
    noArgs' = Constructor (Qualified (Just dataGenericRep) (ProperName "NoArguments"))

    sumCtor :: Type
    sumCtor = TypeConstructor (Qualified (Just dataGenericRep) (ProperName "Sum"))

    inl :: Qualified (ProperName 'ConstructorName)
    inl = Qualified (Just dataGenericRep) (ProperName "Inl")

    inr :: Qualified (ProperName 'ConstructorName)
    inr = Qualified (Just dataGenericRep) (ProperName "Inr")

    productName :: Qualified (ProperName ty)
    productName = Qualified (Just dataGenericRep) (ProperName "Product")

    constructor :: Qualified (ProperName ty)
    constructor = Qualified (Just dataGenericRep) (ProperName "Constructor")

    constructor' :: Expr -> Expr
    constructor' = App (Constructor constructor)

    argument :: Qualified (ProperName ty)
    argument = Qualified (Just dataGenericRep) (ProperName "Argument")

    argument' :: Expr -> Expr
    argument' = App (Constructor argument)

    record :: Qualified (ProperName ty)
    record = Qualified (Just dataGenericRep) (ProperName "Rec")

    record' :: Expr -> Expr
    record' = App (Constructor record)

    field :: Qualified (ProperName ty)
    field = Qualified (Just dataGenericRep) (ProperName "Field")

    field' :: Expr -> Expr
    field' = App (Constructor field)

checkIsWildcard :: MonadError MultipleErrors m => ProperName 'TypeName -> Type -> m ()
checkIsWildcard _ (TypeWildcard _) = return ()
checkIsWildcard tyConNm _ =
  throwError . errorMessage $ ExpectedWildcard tyConNm

deriveEq ::
  forall m. (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveEq mn syns ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  eqFun <- mkEqFunction tyCon
  return [ ValueDeclaration (Ident C.eq) Public [] (unguarded eqFun) ]
  where
    mkEqFunction :: Declaration -> m Expr
    mkEqFunction (DataDeclaration _ _ _ args) = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 x y <$> (addCatch <$> mapM mkCtorClause args)
    mkEqFunction (PositionedDeclaration _ _ d) = mkEqFunction d
    mkEqFunction _ = internalError "mkEqFunction: expected DataDeclaration"

    preludeConj :: Expr -> Expr -> Expr
    preludeConj = App . App (Var (Qualified (Just (ModuleName [ProperName "Data", ProperName "HeytingAlgebra"])) (Ident C.conj)))

    preludeEq :: Expr -> Expr -> Expr
    preludeEq = App . App (Var (Qualified (Just dataEq) (Ident C.eq)))

    addCatch :: [CaseAlternative] -> [CaseAlternative]
    addCatch xs
      | length xs /= 1 = xs ++ [catchAll]
      | otherwise = xs -- Avoid redundant case
      where
      catchAll = CaseAlternative [NullBinder, NullBinder] (unguarded (Literal (BooleanLiteral False)))

    mkCtorClause :: (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
    mkCtorClause (ctorName, tys) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM (replaceAllTypeSynonymsM syns) tys
      let tests = zipWith3 toEqTest (map (Var . Qualified Nothing) identsL) (map (Var . Qualified Nothing) identsR) tys'
      return $ CaseAlternative [caseBinder identsL, caseBinder identsR] (unguarded (conjAll tests))
      where
      caseBinder idents = ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)

    conjAll :: [Expr] -> Expr
    conjAll [] = Literal (BooleanLiteral True)
    conjAll xs = foldl1 preludeConj xs

    toEqTest :: Expr -> Expr -> Type -> Expr
    toEqTest l r ty | Just rec <- objectType ty
                    , Just fields <- decomposeRec rec =
      conjAll
      . map (\((Label str), typ) -> toEqTest (Accessor str l) (Accessor str r) typ)
      $ fields
    toEqTest l r _ = preludeEq l r

deriveOrd ::
  forall m. (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveOrd mn syns ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  compareFun <- mkCompareFunction tyCon
  return [ ValueDeclaration (Ident C.compare) Public [] (unguarded compareFun) ]
  where
    mkCompareFunction :: Declaration -> m Expr
    mkCompareFunction (DataDeclaration _ _ _ args) = do
      x <- freshIdent "x"
      y <- freshIdent "y"
      lamCase2 x y <$> (addCatch . concat <$> mapM mkCtorClauses (splitLast args))
    mkCompareFunction (PositionedDeclaration _ _ d) = mkCompareFunction d
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
    orderingName = Qualified (Just (ModuleName [ProperName "Data", ProperName "Ordering"])) . ProperName

    orderingCtor :: Text -> Expr
    orderingCtor = Constructor . orderingName

    orderingBinder :: Text -> Binder
    orderingBinder name = ConstructorBinder (orderingName name) []

    ordCompare :: Expr -> Expr -> Expr
    ordCompare = App . App (Var (Qualified (Just dataOrd) (Ident C.compare)))

    mkCtorClauses :: ((ProperName 'ConstructorName, [Type]), Bool) -> m [CaseAlternative]
    mkCtorClauses ((ctorName, tys), isLast) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      tys' <- mapM (replaceAllTypeSynonymsM syns) tys
      let tests = zipWith3 toOrdering (map (Var . Qualified Nothing) identsL) (map (Var . Qualified Nothing) identsR) tys'
          extras | not isLast = [ CaseAlternative [ ConstructorBinder (Qualified (Just mn) ctorName) (replicate (length tys) NullBinder)
                                                  , NullBinder
                                                  ]
                                                  (unguarded (orderingCtor "LT"))
                                , CaseAlternative [ NullBinder
                                                  , ConstructorBinder (Qualified (Just mn) ctorName) (replicate (length tys) NullBinder)
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
      caseBinder idents = ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)

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

    toOrdering :: Expr -> Expr -> Type -> Expr
    toOrdering l r ty | Just rec <- objectType ty
                      , Just fields <- decomposeRec rec =
      appendAll
      . map (\((Label str), typ) -> toOrdering (Accessor str l) (Accessor str r) typ)
      $ fields
    toOrdering l r _ = ordCompare l r

deriveNewtype
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> Type
  -> m ([Declaration], Type)
deriveNewtype mn syns ds tyConNm tyConArgs unwrappedTy = do
    checkIsWildcard tyConNm unwrappedTy
    go =<< findTypeDecl tyConNm ds
  where
    go :: Declaration -> m ([Declaration], Type)
    go (DataDeclaration Data name _ _) =
      throwError . errorMessage $ CannotDeriveNewtypeForData name
    go (DataDeclaration Newtype name args dctors) = do
      checkNewtype name dctors
      wrappedIdent <- freshIdent "n"
      unwrappedIdent <- freshIdent "a"
      let (ctorName, [ty]) = head dctors
      ty' <- replaceAllTypeSynonymsM syns ty
      let inst =
            [ ValueDeclaration (Ident "wrap") Public [] $ unguarded $
                Constructor (Qualified (Just mn) ctorName)
            , ValueDeclaration (Ident "unwrap") Public [] $ unguarded $
                lamCase wrappedIdent
                  [ CaseAlternative
                      [ConstructorBinder (Qualified (Just mn) ctorName) [VarBinder unwrappedIdent]]
                      (unguarded (Var (Qualified Nothing unwrappedIdent)))
                  ]
            ]
          subst = zipWith ((,) . fst) args tyConArgs
      return (inst, replaceAllTypeVars subst ty')
    go (PositionedDeclaration _ _ d) = go d
    go _ = internalError "deriveNewtype go: expected DataDeclaration"

findTypeDecl
  :: (MonadError MultipleErrors m)
  => ProperName 'TypeName
  -> [Declaration]
  -> m Declaration
findTypeDecl tyConNm = maybe (throwError . errorMessage $ CannotFindDerivingType tyConNm) return . find isTypeDecl
  where
  isTypeDecl :: Declaration -> Bool
  isTypeDecl (DataDeclaration _ nm _ _) | nm == tyConNm = True
  isTypeDecl (PositionedDeclaration _ _ d) = isTypeDecl d
  isTypeDecl _ = False

lam :: Ident -> Expr -> Expr
lam = Abs . VarBinder

lamNull :: Expr -> Expr
lamNull = lam (Ident "$q") -- TODO: use GenIdent

lamCase :: Ident -> [CaseAlternative] -> Expr
lamCase s = lam s . Case [mkVar s]

lamCase2 :: Ident -> Ident -> [CaseAlternative] -> Expr
lamCase2 s t = lam s . lam t . Case [mkVar s, mkVar t]

mkVarMn :: Maybe ModuleName -> Ident -> Expr
mkVarMn mn = Var . Qualified mn

mkVar :: Ident -> Expr
mkVar = mkVarMn Nothing

-- This function may seem a little obtuse, but it's only this way to ensure
-- that it is injective. Injectivity is important here; without it, we can end
-- up with accidental variable shadowing in the generated code.
labelToIdent :: Label -> Ident
labelToIdent =
  Ident . foldMap (either loneSurrogate char) . decodeStringEither . runLabel
  where
  char '_' = "__"
  char c = T.singleton c
  loneSurrogate x = "_" <> T.pack (show x) <> "_"

objectType :: Type -> Maybe Type
objectType (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Record"))) rec) = Just rec
objectType _ = Nothing

decomposeRec :: Type -> Maybe [(Label, Type)]
decomposeRec = fmap (sortBy (comparing fst)) . go
  where go (RCons str typ typs) = fmap ((str, typ) :) (go typs)
        go REmpty = Just []
        go _ = Nothing

decomposeRec' :: Type -> [(Label, Type)]
decomposeRec' = sortBy (comparing fst) . go
  where go (RCons str typ typs) = (str, typ) : go typs
        go _ = []

deriveFunctor
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> SynonymMap
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveFunctor mn syns ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  mapFun <- mkMapFunction tyCon
  return [ ValueDeclaration (Ident C.map) Public [] (unguarded mapFun) ]
  where
    mkMapFunction :: Declaration -> m Expr
    mkMapFunction (DataDeclaration _ _ tys ctors) = case reverse tys of
      [] -> throwError . errorMessage $ KindsDoNotUnify (FunKind kindType kindType) kindType
      ((iTy, _) : _) -> do
        f <- freshIdent "f"
        m <- freshIdent "m"
        lam f . lamCase m <$> mapM (mkCtorClause iTy f) ctors
    mkMapFunction (PositionedDeclaration _ _ d) = mkMapFunction d
    mkMapFunction _ = internalError "mkMapFunction: expected DataDeclaration"

    mkCtorClause :: Text -> Ident -> (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
    mkCtorClause iTyName f (ctorName, ctorTys) = do
      idents <- replicateM (length ctorTys) (freshIdent "v")
      ctorTys' <- mapM (replaceAllTypeSynonymsM syns) ctorTys
      args <- zipWithM transformArg idents ctorTys'
      let ctor = Constructor (Qualified (Just mn) ctorName)
          rebuilt = foldl App ctor args
          caseBinder = ConstructorBinder (Qualified (Just mn) ctorName) (VarBinder <$> idents)
      return $ CaseAlternative [caseBinder] (unguarded rebuilt)
      where
        fVar = mkVar f
        mapVar = mkVarMn (Just dataFunctor) (Ident C.map)

        -- TODO: deal with type synonyms, ala https://github.com/purescript/purescript/pull/2516
        transformArg :: Ident -> Type -> m Expr
        transformArg ident = fmap (foldr App (mkVar ident)) . goType where

          goType :: Type -> m (Maybe Expr)
          -- argument matches the index type
          goType (TypeVar t) | t == iTyName = return (Just fVar)

          -- records
          goType recTy | Just row <- objectType recTy =
              traverse buildUpdate (decomposeRec' row) >>= (traverse buildRecord . justUpdates)
            where
              justUpdates :: [Maybe (Label, Expr)] -> Maybe [(Label, Expr)]
              justUpdates = foldMap (fmap return)

              buildUpdate :: (Label, Type) -> m (Maybe (Label, Expr))
              buildUpdate (lbl, ty) = do upd <- goType ty
                                         return ((lbl,) <$> upd)

              buildRecord :: [(Label, Expr)] -> m Expr
              buildRecord updates = do arg <- freshIdent "o"
                                       let argVar = mkVar arg
                                           mkAssignment ((Label l), x) = (l, App x (Accessor l argVar))
                                       return (lam arg (ObjectUpdate argVar (mkAssignment <$> updates)))

          -- under a `* -> *`, just assume functor for now
          goType (TypeApp _ t) = fmap (App mapVar) <$> goType t

          -- otherwise do nothing - will fail type checking if type does actually contain index
          goType _ = return Nothing
