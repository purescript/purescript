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
--
-----------------------------------------------------------------------------

module Language.PureScript.Scope (
    usedNames,
    unusedNames,
    firstUnusedName
) where

import Data.Data
import Data.List ((\\), nub)
import Data.Generics (extQ, mkQ, everything)

import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST
import Data.Maybe (mapMaybe)

usedNames :: (Data d) => d -> [Ident]
usedNames val = nub $ everything (++) (mkQ [] namesV `extQ` namesS `extQ` namesB `extQ` namesJS) val
  where
  namesV :: Value -> [Ident]
  namesV (Abs args _) = args
  namesV (Var (Qualified (ModulePath []) name)) = [name]
  namesV _ = []
  namesS :: Statement -> [Ident]
  namesS (VariableIntroduction name _) = [name]
  namesS (For name _ _ _) = [name]
  namesS _ = []
  namesB :: Binder -> [Ident]
  namesB (VarBinder name) = [name]
  namesB _ = []
  namesJS :: JS -> [Ident]
  namesJS (JSVar name) = [name]
  namesJS (JSFunction (Just name) args _) = name : args
  namesJS (JSFunction Nothing args _) = args
  namesJS (JSVariableIntroduction name _) = [name]
  namesJS (JSFor name _ _ _) = [name]
  namesJS _ = []

unusedNames :: (Data d) => d -> [Ident]
unusedNames val =
  let
    allNames = usedNames val
    varNames = map (Ident . ('_' :) . show) ([1..] :: [Int])
  in
    varNames \\ allNames

firstUnusedName :: (Data d) => d -> Int
firstUnusedName val =
  let
    allNames = usedNames val
    varNames = mapMaybe toUnknown allNames
  in
    1 + maximum (0 : varNames)
  where
  toUnknown :: Ident -> Maybe Int
  toUnknown (Ident ('_' : s)) = readMaybe s
  toUnknown _ = Nothing

readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
                [(n, "")] -> Just n
                _ -> Nothing
