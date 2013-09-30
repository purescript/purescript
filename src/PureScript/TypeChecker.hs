-----------------------------------------------------------------------------
--
-- Module      :  PureScript.TypeChecker
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

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module PureScript.TypeChecker (
    module PureScript.TypeChecker.Monad,
    module PureScript.TypeChecker.Kinds,
    module PureScript.TypeChecker.Types,
    typeCheck,
    typeCheckAll,
) where

import PureScript.TypeChecker.Monad
import PureScript.TypeChecker.Kinds
import PureScript.TypeChecker.Types

import Data.List
import Data.Function

import PureScript.Values
import PureScript.Types
import PureScript.Kinds
import PureScript.Declarations

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

typeCheck :: Declaration -> Check ()
typeCheck (DataDeclaration dcs@(DataConstructors
  { typeConstructorName = name
  , typeArguments = args
  , dataConstructors = ctors
  })) = rethrow (("Error in type constructor " ++ name ++ ": ") ++) $ do
  env <- get
  guardWith (name ++ " is already defined") $ not $ M.member name (types env)
  kinds <- kindsOf args (map snd ctors)
  let ctorKind = foldl FunKind Star kinds
  put $ env { types = M.insert name ctorKind (types env) }
  flip mapM_ ctors $ \(dctor, ty) -> rethrow (("Error in data constructor " ++ name ++ ": ") ++) $ do
    env' <- get
    guardWith (dctor ++ " is already defined") $ not $ flip M.member (names env') dctor
    let ctorType = Function [ty] $ foldl TypeApp (TypeConstructor name) (map TypeVar args)
    put $ env' { names = M.insert dctor ctorType (names env) }
typeCheck (ValueDeclaration name val) = rethrow (("Error in declaration " ++ name ++ ": ") ++) $ do
  env <- get
  case M.lookup name (names env) of
    Just ty -> throwError $ name ++ " is already defined"
    Nothing -> do
      ty <- typeOf val
      put (env { names = M.insert name ty (names env) })

typeCheckAll :: [Declaration] -> Check ()
typeCheckAll = mapM_ typeCheck
