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

import qualified Data.Map as M

-- |
-- Type constructor for functions
--
tyFunction :: Type
tyFunction = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Function")

-- |
-- Type constructor for strings
--
tyString :: Type
tyString = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "String")

-- |
-- Type constructor for numbers
--
tyNumber :: Type
tyNumber = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Number")

-- |
-- Type constructor for booleans
--
tyBoolean :: Type
tyBoolean = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Boolean")

-- |
-- Type constructor for arrays
--
tyArray :: Type
tyArray = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Array")

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
  where
  primName name = Qualified (Just $ ModuleName [ProperName "Prim"]) (ProperName name)
