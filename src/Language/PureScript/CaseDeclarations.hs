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
--
-----------------------------------------------------------------------------

module Language.PureScript.CaseDeclarations (
    desugarCases
) where

import Data.List (groupBy)
import Control.Monad (join, unless)
import Control.Monad.Error.Class

import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Declarations
import Language.PureScript.Scope

desugarCases :: [Declaration] -> Either String [Declaration]
desugarCases = fmap join . mapM toDecls . groupBy inSameGroup

inSameGroup :: Declaration -> Declaration -> Bool
inSameGroup (ValueDeclaration ident1 _ _ _) (ValueDeclaration ident2 _ _ _) = ident1 == ident2
inSameGroup _ _ = False

toDecls :: [Declaration] -> Either String [Declaration]
toDecls d@[ValueDeclaration _ [] Nothing _] = return d
toDecls ds@(ValueDeclaration ident bs _ _ : _) = do
  let tuples = map toTuple ds
  unless (all ((== map length bs) . map length . fst) tuples) $
      throwError $ "Argument list lengths differ in declaration " ++ show ident
  return [makeCaseDeclaration ident tuples]
toDecls ds = return ds

toTuple :: Declaration -> ([[Binder]], (Maybe Guard, Value))
toTuple (ValueDeclaration _ bs g val) = (bs, (g, val))
toTuple _ = error "Not a value declaration"

makeCaseDeclaration :: Ident -> [([[Binder]], (Maybe Guard, Value))] -> Declaration
makeCaseDeclaration ident alternatives =
  let
    argPattern = map length . fst . head $ alternatives
    args = take (sum argPattern) $ unusedNames (ident, alternatives)
    vars = map (\arg -> Var (Qualified global arg)) args
    binders = [ (join bs, g, val) | (bs, (g, val)) <- alternatives ]
    value = foldr (\args' ret -> Abs args' ret) (Case vars binders) (rearrange argPattern args)
  in
    ValueDeclaration ident [] Nothing value

rearrange :: [Int] -> [a] -> [[a]]
rearrange [] _ = []
rearrange (n:ns) xs = take n xs : rearrange ns (drop n xs)

