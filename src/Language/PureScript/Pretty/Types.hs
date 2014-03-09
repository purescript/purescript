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
    prettyPrintTypeAtom,
    prettyPrintRow
) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Generics (mkT, everywhere, everywhere')

import Control.Arrow ((<+>))
import Control.PatternArrows
import Control.Monad.Unify

import Language.PureScript.Types
import Language.PureScript.Pretty.Common
import Language.PureScript.Environment

typeLiterals :: Pattern () Type String
typeLiterals = mkPattern match
  where
  match (Object row) = Just $ "{ " ++ prettyPrintRow row ++ " }"
  match (TypeVar var) = Just var
  match (PrettyPrintArray ty) = Just $ "[" ++ prettyPrintType ty ++ "]"
  match (TypeConstructor ctor) = Just $ show ctor
  match (TUnknown (Unknown u)) = Just $ 'u' : show u
  match (Skolem s _) = Just $ 's' : show s
  match (ConstrainedType deps ty) = Just $ "(" ++ intercalate ", " (map (\(pn, ty') -> show pn ++ " " ++ unwords (map prettyPrintTypeAtom ty')) deps) ++ ") => " ++ prettyPrintType ty
  match (SaturatedTypeSynonym name args) = Just $ show name ++ "<" ++ intercalate "," (map prettyPrintTypeAtom args) ++ ">"
  match (PrettyPrintForAll idents ty) = Just $ "forall " ++ unwords idents ++ ". " ++ prettyPrintType ty
  match REmpty = Just "()"
  match row@RCons{} = Just $ '(' : prettyPrintRow row ++ ")"
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
  tailToPs (TUnknown (Unknown u)) = " | u" ++ show u
  tailToPs (TypeVar var) = " | " ++ var
  tailToPs (Skolem s _) = " | s" ++ show s
  tailToPs _ = error "Invalid row tail"
  toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
  toList tys (RCons name ty row) = toList ((name, ty):tys) row
  toList tys r = (tys, r)

typeApp :: Pattern () Type (Type, Type)
typeApp = mkPattern match
  where
  match (TypeApp f x) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () Type (Type, Type)
appliedFunction = mkPattern match
  where
  match (PrettyPrintFunction arg ret) = Just (arg, ret)
  match _ = Nothing

insertPlaceholders :: Type -> Type
insertPlaceholders = everywhere' (mkT convertForAlls) . everywhere (mkT convert)
  where
  convert (TypeApp (TypeApp f arg) ret) | f == tyFunction = PrettyPrintFunction arg ret
  convert (TypeApp a el) | a == tyArray = PrettyPrintArray el
  convert other = other
  convertForAlls (ForAll ident ty _) = go [ident] ty
    where
    go idents (ForAll ident' ty' _) = go (ident' : idents) ty'
    go idents other = PrettyPrintForAll idents other
  convertForAlls other = other

matchTypeAtom :: Pattern () Type String
matchTypeAtom = typeLiterals <+> fmap parens matchType

matchType :: Pattern () Type String
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () Type String
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f ++ " " ++ x ]
                  , [ AssocR appliedFunction $ \arg ret -> arg ++ " -> " ++ ret
                    ]
                  ]

-- |
-- Generate a pretty-printed string representing a Type, as it should appear inside parentheses
--
prettyPrintTypeAtom :: Type -> String
prettyPrintTypeAtom = fromMaybe (error "Incomplete pattern") . pattern matchTypeAtom () . insertPlaceholders


-- |
-- Generate a pretty-printed string representing a Type
--
prettyPrintType :: Type -> String
prettyPrintType = fromMaybe (error "Incomplete pattern") . pattern matchType () . insertPlaceholders
