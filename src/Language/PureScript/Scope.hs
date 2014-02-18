-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Scope
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Utility functions for working with names in scope
--
-----------------------------------------------------------------------------

module Language.PureScript.Scope (
    usedNames,
    unusedNames
) where

import Data.Data
import Data.List ((\\), nub)
import Data.Generics (extQ, mkQ, everything)

import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST

-- |
-- Gather all used names appearing inside a value
--
usedNames :: (Data d) => d -> [Ident]
usedNames val = nub $ everything (++) (mkQ [] namesV `extQ` namesB `extQ` namesJS) val
  where
  namesV :: Value -> [Ident]
  namesV (Abs arg _) = [arg]
  namesV (Var (Qualified Nothing name)) = [name]
  namesV _ = []
  namesB :: Binder -> [Ident]
  namesB (VarBinder name) = [name]
  namesB _ = []
  namesJS :: JS -> [Ident]
  namesJS (JSVar name) = [Ident name]
  namesJS (JSFunction (Just name) args _) = (Ident name) : (Ident `map` args)
  namesJS (JSFunction Nothing args _) = (Ident `map` args)
  namesJS (JSVariableIntroduction name _) = [Ident name]
  namesJS (JSFor name _ _ _) = [Ident name]
  namesJS _ = []

-- |
-- Generate a set of names which are unused inside a value, of the form @_{n}@ for an integer @n@
--
unusedNames :: (Data d) => d -> [Ident]
unusedNames val =
  let
    allNames = usedNames val
    varNames = map (Ident . ('_' :) . show) ([1..] :: [Int])
  in
    varNames \\ allNames
