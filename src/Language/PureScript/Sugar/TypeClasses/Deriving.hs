-- |
-- This module implements the generic deriving elaboration that takes place during desugaring.
--
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import Prelude.Compat

import Control.Arrow (second)
import Control.Monad (replicateM, zipWithM)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class (MonadSupply)

import Data.List (foldl', find, sortBy, unzip5)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker (checkNewtype)
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C

-- | Elaborates deriving instance declarations by code generation.
deriveInstances
  :: (MonadError MultipleErrors m, MonadSupply m)
  => Module
  -> m Module
deriveInstances (Module ss coms mn ds exts) = Module ss coms mn <$> mapM (deriveInstance mn ds) ds <*> pure exts

-- | Takes a declaration, and if the declaration is a deriving TypeInstanceDeclaration,
-- elaborates that into an instance declaration via code generation.
deriveInstance
  :: (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> Declaration
  -> m Declaration
deriveInstance mn ds (TypeInstanceDeclaration nm deps className tys@[ty] DerivedInstance)
  | className == Qualified (Just dataGeneric) (ProperName C.generic)
  , Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveGeneric mn ds tyCon args

  | className == Qualified (Just dataEq) (ProperName "Eq")
  , Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveEq mn ds tyCon

  | className == Qualified (Just dataOrd) (ProperName "Ord")
  , Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveOrd mn ds tyCon

  | className == Qualified (Just dataFunctor) (ProperName "Functor")
  , Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveFunctor mn ds tyCon

deriveInstance mn ds (TypeInstanceDeclaration nm deps className [wrappedTy, unwrappedTy] DerivedInstance)
  | className == Qualified (Just dataNewtype) (ProperName "Newtype")
  , Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor wrappedTy
  , mn == fromMaybe mn mn'
  = do (inst, actualUnwrappedTy) <- deriveNewtype mn ds tyCon args unwrappedTy
       return $ TypeInstanceDeclaration nm deps className [wrappedTy, actualUnwrappedTy] (ExplicitInstance inst)
deriveInstance mn ds (TypeInstanceDeclaration nm deps className [actualTy, repTy] DerivedInstance)
  | className == Qualified (Just dataGenericRep) (ProperName C.generic)
  , Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor actualTy
  , mn == fromMaybe mn mn'
  = do (inst, inferredRepTy) <- deriveGenericRep mn ds tyCon args repTy
       return $ TypeInstanceDeclaration nm deps className [actualTy, inferredRepTy] (ExplicitInstance inst)
deriveInstance _ _ (TypeInstanceDeclaration _ _ className tys DerivedInstance)
  = throwError . errorMessage $ CannotDerive className tys
deriveInstance mn ds (TypeInstanceDeclaration nm deps className tys@(_ : _) NewtypeInstance)
  | Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor (last tys)
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . NewtypeInstanceWithDictionary <$> deriveNewtypeInstance className ds tys tyCon args
deriveInstance _ _ (TypeInstanceDeclaration _ _ className tys NewtypeInstance)
  = throwError . errorMessage $ InvalidNewtypeInstance className tys
deriveInstance mn ds (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> deriveInstance mn ds d
deriveInstance _  _  e = return e

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
   . MonadError MultipleErrors m
  => Qualified (ProperName 'ClassName)
  -> [Declaration]
  -> [Type]
  -> ProperName 'TypeName
  -> [Type]
  -> m Expr
deriveNewtypeInstance className ds tys tyConNm dargs = do
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
             return (DeferredDictionary className (init tys ++ [replaceAllTypeVars subst wrapped']))
    go (PositionedDeclaration _ _ d) = go d
    go _ = throwError . errorMessage $ InvalidNewtypeInstance className tys

    takeReverse :: Int -> [a] -> [a]
    takeReverse n = take n . reverse

    stripRight :: [(Text, Maybe kind)] -> Type -> Maybe Type
    stripRight [] ty = Just ty
    stripRight ((arg, _) : args) (TypeApp t (TypeVar arg'))
      | arg == arg' = stripRight args t
    stripRight _ _ = Nothing

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

deriveGeneric
  :: forall m. (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> m [Declaration]
deriveGeneric mn ds tyConNm dargs = do
  tyCon <- findTypeDecl tyConNm ds
  toSpine <- mkSpineFunction tyCon
  fromSpine <- mkFromSpineFunction tyCon
  let toSignature = mkSignatureFunction tyCon dargs
  return [ ValueDeclaration (Ident C.toSpine) Public [] (Right toSpine)
         , ValueDeclaration (Ident C.fromSpine) Public [] (Right fromSpine)
         , ValueDeclaration (Ident C.toSignature) Public [] (Right toSignature)
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
        return $ CaseAlternative [ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)] (Right (caseResult idents))
        where
        caseResult idents =
          App (prodConstructor (Literal . StringLiteral . showQualified runProperName $ Qualified (Just mn) ctorName))
            . Literal . ArrayLiteral
            $ zipWith toSpineFun (map (Var . Qualified Nothing) idents) tys

      toSpineFun :: Expr -> Type -> Expr
      toSpineFun i r | Just rec <- objectType r =
        lamNull . recordConstructor . Literal . ArrayLiteral
          . map
            (\(str,typ) ->
              Literal $ ObjectLiteral
                [ ("recLabel", Literal (StringLiteral str))
                , ("recValue", toSpineFun (Accessor str i) typ)
                ]
            )
          $ decomposeRec rec
      toSpineFun i _ = lamNull $ App (mkGenVar (Ident C.toSpine)) i
    mkSpineFunction (PositionedDeclaration _ _ d) = mkSpineFunction d
    mkSpineFunction _ = internalError "mkSpineFunction: expected DataDeclaration"

    mkSignatureFunction :: Declaration -> [Type] -> Expr
    mkSignatureFunction (DataDeclaration _ name tyArgs args) classArgs = lamNull . mkSigProd $ map mkProdClause args
      where
      mkSigProd :: [Expr] -> Expr
      mkSigProd =
        App
          (App
            (Constructor (Qualified (Just dataGeneric) (ProperName "SigProd")))
            (Literal (StringLiteral (showQualified runProperName (Qualified (Just mn) name))))
          )
          . Literal
          . ArrayLiteral

      mkSigRec :: [Expr] -> Expr
      mkSigRec = App (Constructor (Qualified (Just dataGeneric) (ProperName "SigRecord"))) . Literal . ArrayLiteral

      proxy :: Type -> Type
      proxy = TypeApp (TypeConstructor (Qualified (Just typesProxy) (ProperName "Proxy")))

      mkProdClause :: (ProperName 'ConstructorName, [Type]) -> Expr
      mkProdClause (ctorName, tys) =
        Literal $ ObjectLiteral
          [ ("sigConstructor", Literal (StringLiteral (showQualified runProperName (Qualified (Just mn) ctorName))))
          , ("sigValues", Literal . ArrayLiteral . map (mkProductSignature . instantiate) $ tys)
          ]

      mkProductSignature :: Type -> Expr
      mkProductSignature r | Just rec <- objectType r =
          lamNull . mkSigRec $
            [ Literal $ ObjectLiteral
                [ ("recLabel", Literal (StringLiteral str))
                , ("recValue", mkProductSignature typ)
                ]
            | (str, typ) <- decomposeRec rec
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
        return $
          CaseAlternative
            [ prodBinder
                [ LiteralBinder (StringLiteral (showQualified runProperName (Qualified (Just mn) ctorName)))
                , LiteralBinder (ArrayLiteral (map VarBinder idents))
                ]
            ]
            . Right
            $ liftApplicative
                (mkJust $ Constructor (Qualified (Just mn) ctorName))
                (zipWith fromSpineFun (map (Var . Qualified Nothing) idents) tys)

      addCatch :: [CaseAlternative] -> [CaseAlternative]
      addCatch = (++ [catchAll])
        where
        catchAll = CaseAlternative [NullBinder] (Right mkNothing)

      fromSpineFun :: Expr -> Type -> Expr
      fromSpineFun e r
        | Just rec <- objectType r
        = App (lamCase (Ident "r") [ mkRecCase (decomposeRec rec)
                                   , CaseAlternative [NullBinder] (Right mkNothing)
                                   ])
              (App e unitVal)
      fromSpineFun e _ = App (mkGenVar (Ident C.fromSpine)) (App e unitVal)

      mkRecCase :: [(Text, Type)] -> CaseAlternative
      mkRecCase rs =
        CaseAlternative
          [ recordBinder [ LiteralBinder (ArrayLiteral (map (VarBinder . Ident . fst) rs)) ] ]
          . Right
          $ liftApplicative (mkRecFun rs) (map (\(x, y) -> fromSpineFun (Accessor "recValue" (mkVar (Ident x))) y) rs)

      mkRecFun :: [(Text, Type)] -> Expr
      mkRecFun xs = mkJust $ foldr (lam . Ident . fst) recLiteral xs
         where recLiteral = Literal . ObjectLiteral $ map (\(s,_) -> (s, mkVar (Ident s))) xs
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
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> Type
  -> m ([Declaration], Type)
deriveGenericRep mn ds tyConNm tyConArgs repTy = do
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
                   [ ValueDeclaration (Ident "to") Public [] $ Right $
                       lamCase x [ CaseAlternative [NullBinder]
                                                   (Right (App toName (Var (Qualified Nothing x))))
                                 ]
                   , ValueDeclaration (Ident "from") Public [] $ Right $
                       lamCase x [ CaseAlternative [NullBinder]
                                                   (Right (App fromName (Var (Qualified Nothing x))))
                                 ]
                   ]
               | otherwise =
                   [ ValueDeclaration (Ident "to") Public [] $ Right $
                       lamCase x (zipWith ($) (map underBinder (sumBinders (length dctors))) to)
                   , ValueDeclaration (Ident "from") Public [] $ Right $
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
        (ctorTy, matchProduct, ctorArgs, matchCtor, mkProduct) <- makeProduct args
        return ( TypeApp (TypeApp (TypeConstructor constructor)
                                  (TypeLevelString (runProperName ctorName)))
                         ctorTy
               , CaseAlternative [ ConstructorBinder constructor [matchProduct] ]
                                 (Right (foldl App (Constructor (Qualified (Just mn) ctorName)) ctorArgs))
               , CaseAlternative [ ConstructorBinder (Qualified (Just mn) ctorName) matchCtor ]
                                 (Right (constructor' mkProduct))
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
    makeArg arg | Just rec <- objectType arg = do
      let fields = decomposeRec rec
      fieldNames <- traverse freshIdent (map fst fields)
      pure ( TypeApp (TypeConstructor record)
               (foldr1 (\f -> TypeApp (TypeApp (TypeConstructor productName) f))
                 (map (\(name, ty) ->
                   TypeApp (TypeApp (TypeConstructor field) (TypeLevelString name)) ty) fields))
           , ConstructorBinder record
               [ foldr1 (\b1 b2 -> ConstructorBinder productName [b1, b2])
                   (map (\ident -> ConstructorBinder field [VarBinder ident]) fieldNames)
               ]
           , Literal . ObjectLiteral $
                zipWith (\(name, _) ident -> (name, Var (Qualified Nothing ident))) fields fieldNames
           , LiteralBinder . ObjectLiteral $
                 zipWith (\(name, _) ident -> (name, VarBinder ident)) fields fieldNames
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
    underExpr f (CaseAlternative b (Right e)) = CaseAlternative b (Right (f e))
    underExpr _ _ = internalError "underExpr: expected Right"

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
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveEq mn ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  eqFun <- mkEqFunction tyCon
  return [ ValueDeclaration (Ident C.eq) Public [] (Right eqFun) ]
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
      catchAll = CaseAlternative [NullBinder, NullBinder] (Right (Literal (BooleanLiteral False)))

    mkCtorClause :: (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
    mkCtorClause (ctorName, tys) = do
      identsL <- replicateM (length tys) (freshIdent "l")
      identsR <- replicateM (length tys) (freshIdent "r")
      let tests = zipWith3 toEqTest (map (Var . Qualified Nothing) identsL) (map (Var . Qualified Nothing) identsR) tys
      return $ CaseAlternative [caseBinder identsL, caseBinder identsR] (Right (conjAll tests))
      where
      caseBinder idents = ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)

    conjAll :: [Expr] -> Expr
    conjAll [] = Literal (BooleanLiteral True)
    conjAll xs = foldl1 preludeConj xs

    toEqTest :: Expr -> Expr -> Type -> Expr
    toEqTest l r ty | Just rec <- objectType ty =
      conjAll
      . map (\(str, typ) -> toEqTest (Accessor str l) (Accessor str r) typ)
      $ decomposeRec rec
    toEqTest l r _ = preludeEq l r

deriveOrd ::
  forall m. (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveOrd mn ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  compareFun <- mkCompareFunction tyCon
  return [ ValueDeclaration (Ident C.compare) Public [] (Right compareFun) ]
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
      catchAll = CaseAlternative [NullBinder, NullBinder] (Right (orderingCtor "EQ"))

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
      let tests = zipWith3 toOrdering (map (Var . Qualified Nothing) identsL) (map (Var . Qualified Nothing) identsR) tys
          extras | not isLast = [ CaseAlternative [ ConstructorBinder (Qualified (Just mn) ctorName) (replicate (length tys) NullBinder)
                                                  , NullBinder
                                                  ]
                                                  (Right (orderingCtor "LT"))
                                , CaseAlternative [ NullBinder
                                                  , ConstructorBinder (Qualified (Just mn) ctorName) (replicate (length tys) NullBinder)
                                                  ]
                                                  (Right (orderingCtor "GT"))
                                ]
                 | otherwise = []
      return $ CaseAlternative [ caseBinder identsL
                               , caseBinder identsR
                               ]
                               (Right (appendAll tests))
             : extras

      where
      caseBinder idents = ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)

    appendAll :: [Expr] -> Expr
    appendAll [] = orderingCtor "EQ"
    appendAll [x] = x
    appendAll (x : xs) = Case [x] [ CaseAlternative [orderingBinder "LT"]
                                                    (Right (orderingCtor "LT"))
                                  , CaseAlternative [orderingBinder "GT"]
                                                    (Right (orderingCtor "GT"))
                                  , CaseAlternative [ NullBinder ]
                                                    (Right (appendAll xs))
                                  ]

    toOrdering :: Expr -> Expr -> Type -> Expr
    toOrdering l r ty | Just rec <- objectType ty =
      appendAll
      . map (\(str, typ) -> toOrdering (Accessor str l) (Accessor str r) typ)
      $ decomposeRec rec
    toOrdering l r _ = ordCompare l r

deriveNewtype
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> Type
  -> m ([Declaration], Type)
deriveNewtype mn ds tyConNm tyConArgs unwrappedTy = do
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
          inst =
            [ ValueDeclaration (Ident "wrap") Public [] $ Right $
                Constructor (Qualified (Just mn) ctorName)
            , ValueDeclaration (Ident "unwrap") Public [] $ Right $
                lamCase wrappedIdent
                  [ CaseAlternative
                      [ConstructorBinder (Qualified (Just mn) ctorName) [VarBinder unwrappedIdent]]
                      (Right (Var (Qualified Nothing unwrappedIdent)))
                  ]
            ]
          subst = zipWith ((,) . fst) args tyConArgs
      return (inst, replaceAllTypeVars subst ty)
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
lam = Abs . Left

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

objectType :: Type -> Maybe Type
objectType (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Record"))) rec) = Just rec
objectType _ = Nothing

decomposeRec :: Type -> [(Text, Type)]
decomposeRec = sortBy (comparing fst) . go
  where go (RCons str typ typs) = (str, typ) : decomposeRec typs
        go _ = []

deriveFunctor
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> ProperName 'TypeName
  -> m [Declaration]
deriveFunctor mn ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  mapFun <- mkMapFunction tyCon
  return [ ValueDeclaration (Ident C.map) Public [] (Right mapFun) ]
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
      args <- zipWithM transformArg idents ctorTys
      let ctor = Constructor (Qualified (Just mn) ctorName)
          rebuilt = foldl App ctor args
          caseBinder = ConstructorBinder (Qualified (Just mn) ctorName) (VarBinder <$> idents)
      return $ CaseAlternative [caseBinder] (Right rebuilt)
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
              traverse buildUpdate (decomposeRec row) >>= (traverse buildRecord . justUpdates)
            where
              justUpdates :: [Maybe (Text, Expr)] -> Maybe [(Text, Expr)]
              justUpdates = foldMap (fmap return)

              buildUpdate :: (Text, Type) -> m (Maybe (Text, Expr))
              buildUpdate (lbl, ty) = do upd <- goType ty
                                         return ((lbl,) <$> upd)

              buildRecord :: [(Text, Expr)] -> m Expr
              buildRecord updates = do arg <- freshIdent "o"
                                       let argVar = mkVar arg
                                           mkAssignment (l, x) = (l, App x (Accessor l argVar))
                                       return (lam arg (ObjectUpdate argVar (mkAssignment <$> updates)))

          -- under a `* -> *`, just assume functor for now
          goType (TypeApp _ t) = fmap (App mapVar) <$> goType t

          -- otherwise do nothing - will fail type checking if type does actually contain index
          goType _ = return Nothing
