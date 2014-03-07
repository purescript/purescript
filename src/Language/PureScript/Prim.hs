-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Prim
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Prim where

import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Types

import qualified Language.PureScript.Constants as C

import qualified Data.Map as M

-- |
-- Construct a ProperName in the Prim module
--
primName :: String -> Qualified ProperName
primName = Qualified (Just $ ModuleName [ProperName C.prim]) . ProperName

-- |
-- Construct a type in the Prim module
--
primTy :: String -> Type
primTy = TypeConstructor . primName

-- |
-- Type constructor for functions
--
tyFunction :: Type
tyFunction = primTy "Function"

-- |
-- Type constructor for strings
--
tyString :: Type
tyString = primTy "String"

-- |
-- Type constructor for numbers
--
tyNumber :: Type
tyNumber = primTy "Number"

-- |
-- Type constructor for booleans
--
tyBoolean :: Type
tyBoolean = primTy "Boolean"

-- |
-- Type constructor for arrays
--
tyArray :: Type
tyArray = primTy "Array"

-- |
-- Smart constructor for function types
--
function :: Type -> Type -> Type
function t1 = TypeApp (TypeApp tyFunction t1)

-- |
-- The primitive types in the external javascript environment with their associated kinds.
--
primTypes :: M.Map (Qualified ProperName) Kind
primTypes = M.fromList [ (primName "Function" , FunKind Star (FunKind Star Star))
                       , (primName "Array"    , FunKind Star Star)
                       , (primName "String"   , Star)
                       , (primName "Number"   , Star)
                       , (primName "Boolean"  , Star) ]
