-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Deriving
-- Copyright   :  (c) Gershom Bazerman 2015
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the generic deriving elaboration that takes place during typechecking.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}

module Language.PureScript.TypeChecker.Deriving (
    elaborateInstance
) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C

-- | Takes an expression, and if the head of the expression is a TypeClassInstanceMemberFunction, elaborates that into an instantiation of the actual member function via code generation.
elaborateInstance :: (Functor m, MonadState CheckState m, MonadError MultipleErrors m) => Expr -> m Expr
elaborateInstance (TypeClassInstanceMemberFunction funName className t) =
    fromMaybe noInstanceError $ lookup (unQualify className, funName) derivingList
        where noInstanceError = throwError $ MultipleErrors [ErrorInInstance className [TypeConstructor t] (SimpleErrorWrapper $ MissingClassMember funName)]
              derivingList = [((ProperName C.generic, Ident C.toSpine), mkSpineFunction t)
                             ,((ProperName C.generic, Ident C.fromSpine), mkFromSpineFunction t)
                             ,((ProperName C.generic, Ident C.toSignature), mkSignatureFunction t)]
elaborateInstance e = return e

mkSpineFunction :: (Functor m, MonadState CheckState m) => Qualified ProperName ->  m Expr
mkSpineFunction t = do
  ctors <- M.toList . dataConstructors <$> getEnv
  let prodConstructor = App (Constructor $ findName "SProd" ctors)
      recordConstructor = App (Constructor $ findName "SRecord" ctors)

      mkCtorClause (ctorName, (_,_,typ,idents)) = CaseAlternative [ConstructorBinder ctorName (map VarBinder idents)] (Right caseResult)
          where caseResult = App (prodConstructor (StringLiteral . runProperName . unQualify $ ctorName)) . ArrayLiteral $ zipWith toSpineFun (map (Var . Qualified Nothing) idents) (argTypes typ)

      toSpineFun :: Expr -> Type -> Expr
      toSpineFun i (ObjectType rec) =
          lamNull . recordConstructor . ArrayLiteral .
              map (\(str,typ) -> ObjectLiteral [("recLabel", StringLiteral str), ("recValue", toSpineFun (Accessor str i) typ)])
              $ decomposeRec rec
      toSpineFun i _ = lamNull $ App (mkGenVar C.toSpine) i

  return . lamCase "$x" . map mkCtorClause . constructorsForType t $ ctors

mkSignatureFunction :: (Functor m, MonadState CheckState m) => Qualified ProperName ->  m Expr
mkSignatureFunction t = do
  ctors <- M.toList . dataConstructors <$> getEnv
  envtyps <- M.toList . types <$> getEnv
  let mkSigProd = App (Constructor $ findName "SigProd" ctors) . ArrayLiteral
      mkSigRec =  App (Constructor $ findName "SigRecord" ctors) . ArrayLiteral

      mkProdClause (ctorName, (_,_,typ,_)) = ObjectLiteral [("sigConstructor",StringLiteral (runProperName . unQualify $ ctorName)),("sigValues", ArrayLiteral . map mkProductSignature . argTypes $ typ)]

      mkProductSignature (ObjectType rec) =
          lamNull . mkSigRec .
              map (\(str,typ) -> ObjectLiteral [("recLabel", StringLiteral str), ("recValue", mkProductSignature typ)])
              $ decomposeRec rec
      mkProductSignature typ = lamNull $ App (mkGenVar C.toSignature)
                               (TypedValue False (mkGenVar "anyProxy") (TypeApp (TypeConstructor $ findName "Proxy" envtyps) typ))

  return . lamNull . mkSigProd . map mkProdClause . constructorsForType t $ ctors

mkFromSpineFunction :: (Functor m, MonadState CheckState m) => Qualified ProperName ->  m Expr
mkFromSpineFunction t = do
  ctors <- M.toList . dataConstructors <$> getEnv
  let
      mkJust    = App (Constructor $ findName "Just" ctors)
      mkNothing = Constructor $ findName "Nothing" ctors

      mkAlternative (ctor@(Qualified _ (ProperName ctorName)), (_,_,typ,idents)) =
          CaseAlternative [ConstructorBinder (findName "SProd" ctors) [StringBinder ctorName, ArrayBinder (map VarBinder idents)]]
                          . Right $ liftApplicative (mkJust $ Constructor ctor) (zipWith fromSpineFun (map (Var . (Qualified Nothing))idents) (argTypes typ))

      fromSpineFun e (ObjectType rec) = App (lamCase "r" [mkRecCase (decomposeRec rec), CaseAlternative [NullBinder] (Right mkNothing)]) (App e (mkPrelVar "unit"))

      fromSpineFun e _ = App (mkGenVar C.fromSpine) (App e (mkPrelVar "unit"))

      mkRecCase rs = CaseAlternative [ConstructorBinder (findName "SRecord" ctors) [ArrayBinder (map (VarBinder . Ident . fst) rs)]] . Right $ liftApplicative (mkRecFun rs) (map (\(x,y) -> fromSpineFun (Accessor "recValue" (mkVar x)) y) rs)

      mkRecFun :: [(String,Type)] -> Expr
      mkRecFun xs = mkJust $ foldr (\s e -> lam s e) recLiteral (map fst xs)
         where recLiteral = ObjectLiteral $ map (\(s,_) -> (s,mkVar s)) xs

  return . lamCase "$x" $ map mkAlternative (constructorsForType t ctors) ++ [CaseAlternative [NullBinder] (Right mkNothing)]

-- Helpers

pattern ObjectType rec = TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object"))) rec

lam :: String -> Expr -> Expr
lam s = Abs (Left (Ident s))

lamNull :: Expr -> Expr
lamNull = lam "$q"

lamCase :: String -> [CaseAlternative] -> Expr
lamCase s = lam s . Case [mkVar s]

liftApplicative :: Expr -> [Expr] -> Expr
liftApplicative = foldl' (\x e -> App (App (mkPrelVar "apply") x) e)

mkVarMn :: Maybe ModuleName -> String -> Expr
mkVarMn mn s = Var (Qualified mn (Ident s))

mkVar :: String -> Expr
mkVar s = mkVarMn Nothing s

mkPrelVar :: String -> Expr
mkPrelVar s = mkVarMn (Just (ModuleName [ProperName C.prelude])) s

mkGenVar :: String -> Expr
mkGenVar s = mkVarMn (Just (ModuleName [ProperName "Data", ProperName C.generic])) s

decomposeRec :: Type -> [(String, Type)]
decomposeRec = sortBy (comparing fst) . go
    where go (RCons str typ typs) = (str, typ) : decomposeRec typs
          go  _ = []

argTypes :: Type -> [Type]
argTypes (TypeApp (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Function"))) a) b) = a : argTypes b
argTypes _ = []

findName :: String -> [(Qualified ProperName, a)] -> Qualified ProperName
findName s lst = fromMaybe (Qualified Nothing (ProperName s)) . find ((== ProperName s) . unQualify) . map fst $ lst

unQualify :: Qualified a -> a
unQualify (Qualified _ x) = x

constructorsForType :: Qualified ProperName -> [(Qualified ProperName, (a, ProperName, b, c))] -> [(Qualified ProperName, (a, ProperName, b, c))]
constructorsForType t = filter (\(Qualified mn _ ,(_,typeName,_,_)) -> t == Qualified mn typeName)