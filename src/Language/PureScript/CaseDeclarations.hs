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
    args = map (map (('_' :) . show)) $ toArgs argPattern
    obj = ObjectLiteral $ concatMap (map (\arg -> (arg, Var (Qualified global (Ident arg))))) args
    objBinders = [ (applyGuard g $ ObjectBinder (join $ zipWith zip args bs), val) | (bs, (g, val)) <- alternatives ]
    value = foldr (\args' ret -> Abs (map Ident args') ret) (Case obj objBinders) args
  in
    ValueDeclaration ident [] Nothing value

applyGuard :: Maybe Guard -> Binder -> Binder
applyGuard Nothing = id
applyGuard (Just g) = GuardedBinder g

toArgs :: [Int] -> [[Int]]
toArgs = go 1
  where
  go _ [] = []
  go start (n:ns) = [start..start + n - 1] : go (start + n) ns

