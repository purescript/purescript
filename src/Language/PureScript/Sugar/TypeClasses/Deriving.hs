{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module implements the generic deriving elaboration that takes place during desugaring.
--
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import Prelude.Compat

import Control.Arrow (second)
import Control.Monad (replicateM)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class (MonadSupply)

import Data.List (foldl', find, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
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
  | className == Qualified (Just (ModuleName [ ProperName "Data", ProperName "Eq" ])) (ProperName "Eq")
  , Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveEq mn ds tyCon
  | className == Qualified (Just (ModuleName [ ProperName "Data", ProperName "Ord" ])) (ProperName "Ord")
  , Just (Qualified mn' tyCon, _) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveOrd mn ds tyCon
deriveInstance _ _ (TypeInstanceDeclaration _ _ className tys DerivedInstance)
  = throwError . errorMessage $ CannotDerive className tys
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

dataGeneric :: ModuleName
dataGeneric = ModuleName [ ProperName "Data", ProperName "Generic" ]

dataMaybe :: ModuleName
dataMaybe = ModuleName [ ProperName "Data", ProperName "Maybe" ]

typesProxy :: ModuleName
typesProxy = ModuleName [ ProperName "Type", ProperName "Proxy" ]

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

      mkRecCase :: [(String, Type)] -> CaseAlternative
      mkRecCase rs =
        CaseAlternative
          [ recordBinder [ LiteralBinder (ArrayLiteral (map (VarBinder . Ident . fst) rs)) ] ]
          . Right
          $ liftApplicative (mkRecFun rs) (map (\(x, y) -> fromSpineFun (Accessor "recValue" (mkVar (Ident x))) y) rs)

      mkRecFun :: [(String, Type)] -> Expr
      mkRecFun xs = mkJust $ foldr lam recLiteral (map (Ident . fst) xs)
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
    preludeEq = App . App (Var (Qualified (Just (ModuleName [ProperName "Data", ProperName "Eq"])) (Ident C.eq)))

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

    orderingName :: String -> Qualified (ProperName a)
    orderingName = Qualified (Just (ModuleName [ProperName "Data", ProperName "Ordering"])) . ProperName

    orderingCtor :: String -> Expr
    orderingCtor = Constructor . orderingName

    orderingBinder :: String -> Binder
    orderingBinder name = ConstructorBinder (orderingName name) []

    ordCompare :: Expr -> Expr -> Expr
    ordCompare = App . App (Var (Qualified (Just (ModuleName [ProperName "Data", ProperName "Ord"])) (Ident C.compare)))

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

decomposeRec :: Type -> [(String, Type)]
decomposeRec = sortBy (comparing fst) . go
  where go (RCons str typ typs) = (str, typ) : decomposeRec typs
        go _ = []
