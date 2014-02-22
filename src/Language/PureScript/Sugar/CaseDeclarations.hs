-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CaseDeclarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces top-level binders with
-- case expressions.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.CaseDeclarations (
    desugarCases,
    desugarCasesModule
) where

import Data.List (groupBy)
import Data.Generics (mkT, everywhere)

import Control.Applicative ((<$>))
import Control.Monad (forM, join, unless)
import Control.Monad.Error.Class

import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Declarations
import Language.PureScript.Scope

-- |
-- Replace all top-level binders in a module with case expressions.
--
desugarCasesModule :: [Module] -> Either String [Module]
desugarCasesModule ms = forM ms $ \(Module name ds) -> Module name <$> (desugarCases . desugarAbs $ ds)

desugarAbs :: [Declaration] -> [Declaration]
desugarAbs = everywhere (mkT replace)
  where
  replace (Abs (Right binder) val) =
    let
      ident = head $ unusedNames (binder, val)
    in
      Abs (Left ident) $ Case [Var (Qualified Nothing ident)] [([binder], Nothing, val)]
  replace other = other

-- |
-- Replace all top-level binders with case expressions.
--
desugarCases :: [Declaration] -> Either String [Declaration]
desugarCases = fmap join . mapM toDecls . groupBy inSameGroup

inSameGroup :: Declaration -> Declaration -> Bool
inSameGroup (ValueDeclaration ident1 _ _ _) (ValueDeclaration ident2 _ _ _) = ident1 == ident2
inSameGroup _ _ = False

toDecls :: [Declaration] -> Either String [Declaration]
toDecls d@[ValueDeclaration _ [] Nothing _] = return d
toDecls ds@(ValueDeclaration ident bs _ _ : _) = do
  let tuples = map toTuple ds
  unless (all ((== length bs) . length . fst) tuples) $
      throwError $ "Argument list lengths differ in declaration " ++ show ident
  return [makeCaseDeclaration ident tuples]
toDecls ds = return ds

toTuple :: Declaration -> ([Binder], (Maybe Guard, Value))
toTuple (ValueDeclaration _ bs g val) = (bs, (g, val))
toTuple _ = error "Not a value declaration"

makeCaseDeclaration :: Ident -> [([Binder], (Maybe Guard, Value))] -> Declaration
makeCaseDeclaration ident alternatives =
  let
    argPattern = length . fst . head $ alternatives
    args = take argPattern $ unusedNames (ident, alternatives)
    vars = map (Var . Qualified Nothing) args
    binders = [ (bs, g, val) | (bs, (g, val)) <- alternatives ]
    value = foldr (\arg ret -> Abs (Left arg) ret) (Case vars binders) args
  in
    ValueDeclaration ident [] Nothing value

