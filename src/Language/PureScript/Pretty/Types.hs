-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for Types
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Types (
    prettyPrintType,
    prettyPrintRow
) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Control.Arrow ((<+>))
import Control.PatternArrows
import Control.Monad.Unify

import Language.PureScript.Types
import Language.PureScript.Pretty.Common

typeLiterals :: Pattern () Type String
typeLiterals = mkPattern match
  where
  match (Object row) = Just $ "{ " ++ prettyPrintType row ++ " }"
  match (TypeVar var) = Just var
  match (TypeApp arr ty) | arr == tyArray = Just $ "[" ++ prettyPrintType ty ++ "]"
  match (TypeConstructor ctor) = Just $ show ctor
  match (TUnknown (TypedUnknown (Unknown u))) = Just $ 'u' : show u
  match (Skolem s) = Just $ 's' : show s
  match (ConstrainedType deps ty) = Just $ "(" ++ intercalate "," (map (\(pn, ty') -> show pn ++ " (" ++ prettyPrintType ty' ++ ")") deps) ++ ") => " ++ prettyPrintType ty
  match (SaturatedTypeSynonym name args) = Just $ show name ++ "<" ++ intercalate "," (map prettyPrintType args) ++ ">"
  match (ForAll ident ty) = Just $ "forall " ++ ident ++ ". " ++ prettyPrintType ty
  match REmpty = Just $ prettyPrintRow REmpty
  match row@(RCons _ _ _) = Just $ prettyPrintRow row
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRow :: Type -> String
prettyPrintRow = (\(tys, rest) -> intercalate ", " (map (uncurry nameAndTypeToPs) tys) ++ tailToPs rest) . toList []
  where
  nameAndTypeToPs :: String -> Type -> String
  nameAndTypeToPs name ty = name ++ " :: " ++ prettyPrintType ty
  tailToPs :: Type -> String
  tailToPs REmpty = ""
  tailToPs (TUnknown (TypedUnknown (Unknown u))) = " | u" ++ show u
  tailToPs (TypeVar var) = " | " ++ var
  tailToPs (Skolem s) = " | s" ++ show s
  tailToPs _ = error "Invalid row tail"
  toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
  toList tys (RCons name ty row) = toList ((name, ty):tys) row
  toList tys r = (tys, r)

typeApp :: Pattern () Type (Type, Type)
typeApp = mkPattern match
  where
  match (TypeApp f x) = Just (f, x)
  match _ = Nothing

singleArgumentFunction :: Pattern () Type (Type, Type)
singleArgumentFunction = mkPattern match
  where
  match (TypeApp (TypeApp t arg) ret) | t == tyFunction = Just (arg, ret)
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing a Type
--
prettyPrintType :: Type -> String
prettyPrintType = fromMaybe (error "Incomplete pattern") . pattern matchType ()
  where
  matchType :: Pattern () Type String
  matchType = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchType)
  operators :: OperatorTable () Type String
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f ++ " " ++ x ]
                  , [ AssocR singleArgumentFunction $ \arg ret -> arg ++ " -> " ++ ret
                    ]
                  ]
