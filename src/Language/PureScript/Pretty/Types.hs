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
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Types (
    prettyPrintType,
    prettyPrintRow,
    prettyPrintPolyType
) where

import Data.Maybe (fromMaybe)
import Data.List (intersperse, intercalate)
import qualified Control.Arrow as A
import Control.Arrow ((<+>))
import qualified Data.Map as M
import Control.Applicative

import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Pretty.Common

typeLiterals :: Pattern () Type String
typeLiterals = mkPattern match
  where
  match Number = Just "Number"
  match String = Just "String"
  match Boolean = Just "Boolean"
  match (Array ty) = Just $ "[" ++ prettyPrintType ty ++ "]"
  match (Object row) = Just $ "{ " ++ prettyPrintRow row ++ " }"
  match (TypeVar var) = Just var
  match (TypeConstructor ctor) = Just $ show ctor
  match (TUnknown u) = Just $ 'u' : show u
  match (SaturatedTypeSynonym name args) = Just $ show name ++ "<" ++ intercalate "," (map prettyPrintType args) ++ ">"
  match _ = Nothing

prettyPrintRow :: Row -> String
prettyPrintRow = (\(tys, tail) -> intercalate ", " (map (uncurry nameAndTypeToPs) tys) ++ tailToPs tail) . toList []
  where
  nameAndTypeToPs :: String -> Type -> String
  nameAndTypeToPs name ty = name ++ " :: " ++ prettyPrintType ty
  tailToPs :: Row -> String
  tailToPs REmpty = ""
  tailToPs (RUnknown u) = " | " ++ show u
  tailToPs (RowVar var) = " | " ++ var
  toList :: [(String, Type)] -> Row -> ([(String, Type)], Row)
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
  match (Function [arg] ret) = Just (arg, ret)
  match _ = Nothing

function :: Pattern () Type ([Type], Type)
function = mkPattern match
  where
  match (Function args ret) = Just (args, ret)
  match _ = Nothing

prettyPrintType :: Type -> String
prettyPrintType = fromMaybe (error "Incomplete pattern") . pattern matchType ()
  where
  matchType :: Pattern () Type String
  matchType = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchType)
  operators :: OperatorTable () Type String
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f ++ " " ++ x ]
                  , [ AssocR singleArgumentFunction $ \arg ret -> arg ++ " -> " ++ ret
                    , Wrap function $ \args ret -> "(" ++ intercalate ", " (map prettyPrintType args) ++ ") -> " ++ ret
                    ]
                  ]

prettyPrintPolyType :: PolyType -> String
prettyPrintPolyType (PolyType [] ty) = prettyPrintType ty
prettyPrintPolyType (PolyType idents ty) = "forall " ++ unwords idents ++ ". " ++ prettyPrintType ty
