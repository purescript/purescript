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
    usedNamesDecl,
    usedNamesValue,
    usedNamesBinder,
    usedNamesCaseAlternative,
    usedNamesDoNotationElement,
    unusedNames
) where

import Data.List ((\\), nub)

import Language.PureScript.Declarations
import Language.PureScript.Names

usedNames :: (Declaration -> [Ident], Value -> [Ident], Binder -> [Ident], CaseAlternative -> [Ident], DoNotationElement -> [Ident])
usedNames = everythingOnValues (++) f g h (const []) (const [])
  where
  f :: Declaration -> [Ident]
  f (ValueDeclaration name _ _ _ _) = [name]
  f _ = []

  g :: Value -> [Ident]
  g (Abs (Left arg) _) = [arg]
  g (Var (Qualified Nothing name)) = [name]
  g _ = []

  h :: Binder -> [Ident]
  h (VarBinder name) = [name]
  h _ = []

-- |
-- Gather all used names appearing inside a declaration
--
usedNamesDecl :: Declaration -> [Ident]
usedNamesDecl = let (f, _, _, _, _) = usedNames in nub . f

-- |
-- Gather all used names appearing inside a value
--
usedNamesValue :: Value -> [Ident]
usedNamesValue = let (_, f, _, _, _) = usedNames in nub . f

-- |
-- Gather all used names appearing inside a binder
--
usedNamesBinder :: Binder -> [Ident]
usedNamesBinder = let (_, _, f, _, _) = usedNames in nub . f

-- |
-- Gather all used names appearing inside a case alternative
--
usedNamesCaseAlternative :: CaseAlternative -> [Ident]
usedNamesCaseAlternative = let (_, _, _, f, _) = usedNames in nub . f

-- |
-- Gather all used names appearing inside a do notation element
--
usedNamesDoNotationElement :: DoNotationElement -> [Ident]
usedNamesDoNotationElement = let (_, _, _, _, f) = usedNames in nub . f

-- |
-- Generate a set of names which are unused inside a value, of the form @_{n}@ for an integer @n@
--
unusedNames :: [Ident] -> [Ident]
unusedNames allNames =
  let
    varNames = map (Ident . ('_' :) . show) ([1..] :: [Int])
  in
    varNames \\ allNames
