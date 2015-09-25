-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.ObjectWildcards
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.ObjectWildcards (
  desugarObjectConstructors
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow (second)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Data.List (partition)
import Data.Maybe (isJust, fromJust, catMaybes)

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names

desugarObjectConstructors :: forall m. (Applicative m, MonadSupply m, MonadError MultipleErrors m) => Module -> m Module
desugarObjectConstructors (Module ss coms mn ds exts) = Module ss coms mn <$> mapM desugarDecl ds <*> pure exts
  where

  desugarDecl :: Declaration -> m Declaration
  (desugarDecl, _, _) = everywhereOnValuesM return desugarExpr return

  desugarExpr :: Expr -> m Expr
  desugarExpr (ObjectConstructor ps) = wrapLambda ObjectLiteral ps
  desugarExpr (ObjectUpdater (Just obj) ps) = wrapLambda (ObjectUpdate obj) ps
  desugarExpr (ObjectUpdater Nothing ps) = do
    obj <- Ident <$> freshName
    Abs (Left obj) <$> wrapLambda (ObjectUpdate (Var (Qualified Nothing obj))) ps
  desugarExpr (ObjectGetter prop) = do
    arg <- Ident <$> freshName
    return $ Abs (Left arg) (Accessor prop (Var (Qualified Nothing arg)))
  desugarExpr e = return e

  wrapLambda :: ([(String, Expr)] -> Expr) -> [(String, Maybe Expr)] -> m Expr
  wrapLambda mkVal ps =
    let (props, args) = partition (isJust . snd) ps
    in if null args
       then return . mkVal $ second fromJust `map` props
       else do
        (args', ps') <- unzip <$> mapM mkProp ps
        return $ foldr (Abs . Left) (mkVal ps') (catMaybes args')

  mkProp :: (String, Maybe Expr) -> m (Maybe Ident, (String, Expr))
  mkProp (name, Just e) = return (Nothing, (name, e))
  mkProp (name, Nothing) = do
    arg <- Ident <$> freshName
    return (Just arg, (name, Var (Qualified Nothing arg)))
