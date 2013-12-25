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

desugarCases :: [Declaration] -> Either String [Declaration]
desugarCases = fmap join . mapM toDecls . groupBy inSameGroup

inSameGroup :: Declaration -> Declaration -> Bool
inSameGroup (ValueDeclaration ident1 _ _) (ValueDeclaration ident2 _ _) = ident1 == ident2
inSameGroup _ _ = False

toDecls :: [Declaration] -> Either String [Declaration]
toDecls d@[ValueDeclaration _ [] _] = return d
toDecls ds@(ValueDeclaration ident bs _ : _) = do
  let pairs = map toPair ds
  unless (all ((== length bs) . length . fst) pairs) $
      throwError $ "Argument list lengths differ in declaration " ++ show pairs
  return [makeCaseDeclaration ident pairs]
toDecls ds = return ds

toPair :: Declaration -> ([Binder], Value)
toPair (ValueDeclaration _ bs val) = (bs, val)
toPair _ = error "Not a value declaration"

makeCaseDeclaration :: Ident -> [([Binder], Value)] -> Declaration
makeCaseDeclaration ident alternatives =
  let
    numArgs = length (fst . head $ alternatives)
    args = map (('_' :) . show) [1..numArgs]
    obj = ObjectLiteral $ map (\arg -> (arg, Var (Qualified global (Ident arg)))) args
    objBinders = [ (ObjectBinder (zip args bs), val) | (bs, val) <- alternatives ]
    value = foldr (\arg ret -> Abs [Ident arg] ret) (Case obj objBinders) args
  in
    ValueDeclaration ident [] value

