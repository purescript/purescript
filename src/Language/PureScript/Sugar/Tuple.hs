-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Tuple
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Andrey Popp <8mayday@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which:
--
-- * replaces TupleLiteral with ObjectLiteral wrapped in a Tuple newtype of the
--   corresponding arity.
-- * replaces TupleBinder with ObjectBinder wrapped in a Tuple newtype of the
--   corresponding arity.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.Tuple (
    desugarTupleModule
) where

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Errors
import Language.PureScript.Supply

import qualified Language.PureScript.Constants as C

import Control.Applicative
import Control.Monad.Trans.Class

desugarTupleModule :: Module -> SupplyT (Either ErrorStack) Module
desugarTupleModule (Module mn ds exts) = Module mn <$> mapM desugarTuple ds <*> pure exts

desugarTuple :: Declaration -> SupplyT (Either ErrorStack) Declaration
desugarTuple (PositionedDeclaration pos d) = (PositionedDeclaration pos) <$> (rethrowWithPosition pos $ desugarTuple d)
desugarTuple d =
  let (f, _, _) = everywhereOnValuesM return replaceValue replaceBinder
  in f d
  where

  replaceValue :: Expr -> SupplyT (Either ErrorStack) Expr
  replaceValue (TupleLiteral vals) | length vals > 10 = maxArityError
  replaceValue (TupleLiteral vals) | length vals < 2 = minArityError
  replaceValue (TupleLiteral vals) = do
    let arity = length vals
    let ctor = Constructor $ tupleProperName arity
    let value = ObjectLiteral $ zipWithRowNames vals
    return $ App ctor value
  replaceValue (PositionedValue pos v) = PositionedValue pos <$> rethrowWithPosition pos (replaceValue v)
  replaceValue other = return other

  replaceBinder :: Binder -> SupplyT (Either ErrorStack) Binder
  replaceBinder (TupleBinder binders) | length binders > 10 = maxArityError
  replaceBinder (TupleBinder binders) | length binders < 2 = minArityError
  replaceBinder (TupleBinder binders) = do
    let arity = length binders
    let binder = ObjectBinder $ zipWithRowNames binders
    return $ ConstructorBinder (tupleProperName arity) [binder]
  replaceBinder (PositionedBinder pos b) = PositionedBinder pos <$> rethrowWithPosition pos (replaceBinder b)
  replaceBinder other = return other

  tupleProperName :: Int -> Qualified ProperName
  tupleProperName 2 = preludeIdent C.tuple2
  tupleProperName 3 = preludeIdent C.tuple3
  tupleProperName 4 = preludeIdent C.tuple4
  tupleProperName 5 = preludeIdent C.tuple5
  tupleProperName 6 = preludeIdent C.tuple6
  tupleProperName 7 = preludeIdent C.tuple7
  tupleProperName 8 = preludeIdent C.tuple8
  tupleProperName 9 = preludeIdent C.tuple9
  tupleProperName 10 = preludeIdent C.tuple10
  tupleProperName _ = error "impossible tupleProperName argument"

  preludeIdent :: String -> Qualified ProperName
  preludeIdent ident = Qualified (Just prelude) (ProperName ident)

  prelude :: ModuleName
  prelude = ModuleName [ProperName C.prelude]

  zipWithRowNames :: [a] -> [(String, a)]
  zipWithRowNames items =
    let rowNames = (\idx -> '_':(show idx)) <$> ([1..] :: [Int]) in
    zip rowNames items

  minArityError :: SupplyT (Either ErrorStack) a
  minArityError = lift $ Left $ mkErrorStack "Tuples of arity less than 2 are not supported" Nothing

  maxArityError :: SupplyT (Either ErrorStack) a
  maxArityError = lift $ Left $ mkErrorStack "Tuples of arity greater than 10 are not supported" Nothing
