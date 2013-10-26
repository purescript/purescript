-----------------------------------------------------------------------------
--
-- Module      :  PureScript.CodeGen.Externs
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

{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module PureScript.CodeGen.Externs (
    externToPs
) where

import Data.Maybe (fromMaybe)
import Data.List (intersperse, intercalate)
import qualified Control.Arrow as A
import Control.Arrow ((<+>))
import qualified Data.Map as M
import Control.Applicative

import PureScript.Values
import PureScript.Types
import PureScript.Names
import PureScript.Declarations
import PureScript.TypeChecker.Monad
import PureScript.CodeGen.Common

externToPs :: Environment -> Declaration -> Maybe String
externToPs env (ValueDeclaration name _) = do
  (ty, _) <- M.lookup name $ names env
  return $ "extern " ++ show name ++ " :: " ++ polyTypeToPs ty
externToPs env (ExternDeclaration name ty) =
  return $ "extern " ++ show name ++ " :: " ++ polyTypeToPs ty
externToPs env (TypeSynonymDeclaration name args ty) =
  return $ "type " ++ name ++ " " ++ intercalate " " args ++ " = " ++ typeToPs ty
externToPs _ _ = Nothing

typeLiterals :: Pattern Type String
typeLiterals = Pattern $ A.Kleisli match
  where
  match Number = Just "Number"
  match String = Just "String"
  match Boolean = Just "Boolean"
  match (Array ty) = Just $ "[" ++ typeToPs ty ++ "]"
  match (Object row) = Just $ "{ " ++ rowToPs row ++ " }"
  match (TypeVar var) = Just var
  match (TypeConstructor ctor) = Just ctor
  match (TUnknown u) = Just $ show  u
  match _ = Nothing

rowToPs :: Row -> String
rowToPs = (\(tys, tail) -> intercalate "; " (map (uncurry nameAndTypeToPs) tys) ++ tailToPs tail) . toList
  where
  nameAndTypeToPs :: String -> Type -> String
  nameAndTypeToPs name ty = name ++ " :: " ++ typeToPs ty
  tailToPs :: Row -> String
  tailToPs REmpty = ""
  tailToPs (RUnknown u) = show u
  tailToPs (RowVar var) = " | " ++ var
  toList :: Row -> ([(String, Type)], Row)
  toList (RCons name ty row) = let (tys, rest) = toList row
                               in ((name, ty):tys, rest)
  toList r = ([], r)

typeApp :: Pattern Type (Type, Type)
typeApp = Pattern $ A.Kleisli match
  where
  match (TypeApp f x) = Just (f, x)
  match _ = Nothing

singleArgumentFunction :: Pattern Type (Type, Type)
singleArgumentFunction = Pattern $ A.Kleisli match
  where
  match (Function [arg] ret) = Just (arg, ret)
  match _ = Nothing

function :: Pattern Type ([Type], Type)
function = Pattern $ A.Kleisli match
  where
  match (Function args ret) = Just (args, ret)
  match _ = Nothing

typeToPs :: Type -> String
typeToPs = fromMaybe (error "Incomplete pattern") . pattern matchType
  where
  matchType :: Pattern Type String
  matchType = buildPrettyPrinter operators (typeLiterals <+> fmap parens matchType)
  operators :: OperatorTable Type String
  operators =
    OperatorTable $ [ [ AssocL typeApp $ \f x -> f ++ " " ++ x ]
                    , [ AssocR singleArgumentFunction $ \arg ret -> arg ++ " -> " ++ ret
                      , Wrap function $ \args ret -> "(" ++ intercalate ", " (map typeToPs args) ++ ") -> " ++ ret
                      ]
                    ]

polyTypeToPs :: PolyType -> String
polyTypeToPs (PolyType [] ty) = typeToPs ty
polyTypeToPs (PolyType idents ty) = "forall " ++ intercalate " " idents ++ ". " ++ typeToPs ty
