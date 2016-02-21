{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.ObjectWildcards (
  desugarObjectConstructors
) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Data.List (partition)
import Data.Maybe (catMaybes)

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names

desugarObjectConstructors :: forall m. (Applicative m, MonadSupply m, MonadError MultipleErrors m) => Module -> m Module
desugarObjectConstructors (Module ss coms mn ds exts) = Module ss coms mn <$> mapM desugarDecl ds <*> pure exts
  where

  desugarDecl :: Declaration -> m Declaration
  desugarDecl (PositionedDeclaration pos com d) = rethrowWithPosition pos $ PositionedDeclaration pos com <$> desugarDecl d
  desugarDecl other = f other
    where
    (f, _, _) = everywhereOnValuesTopDownM return desugarExpr return

  desugarExpr :: Expr -> m Expr
  desugarExpr AnonymousArgument = throwError . errorMessage $ IncorrectAnonymousArgument
  desugarExpr (Parens b)
    | b' <- stripPositionInfo b
    , BinaryNoParens op val u <- b'
    , isAnonymousArgument u = return $ OperatorSection op (Left val)
    | b' <- stripPositionInfo b
    , BinaryNoParens op u val <- b'
    , isAnonymousArgument u = return $ OperatorSection op (Right val)
  desugarExpr (ObjectLiteral ps) = wrapLambda ObjectLiteral ps
  desugarExpr (ObjectUpdate u ps) | isAnonymousArgument u = do
    obj <- freshIdent'
    Abs (Left obj) <$> wrapLambda (ObjectUpdate (Var (Qualified Nothing obj))) ps
  desugarExpr (ObjectUpdate obj ps) = wrapLambda (ObjectUpdate obj) ps
  desugarExpr (Accessor prop u) | isAnonymousArgument u = do
    arg <- freshIdent'
    return $ Abs (Left arg) (Accessor prop (Var (Qualified Nothing arg)))
  desugarExpr e = return e

  wrapLambda :: ([(String, Expr)] -> Expr) -> [(String, Expr)] -> m Expr
  wrapLambda mkVal ps =
    let (args, props) = partition (isAnonymousArgument . snd) ps
    in if null args
       then return $ mkVal props
       else do
        (args', ps') <- unzip <$> mapM mkProp ps
        return $ foldr (Abs . Left) (mkVal ps') (catMaybes args')

  stripPositionInfo :: Expr -> Expr
  stripPositionInfo (PositionedValue _ _ e) = stripPositionInfo e
  stripPositionInfo e = e

  isAnonymousArgument :: Expr -> Bool
  isAnonymousArgument AnonymousArgument = True
  isAnonymousArgument (PositionedValue _ _ e) = isAnonymousArgument e
  isAnonymousArgument _ = False

  mkProp :: (String, Expr) -> m (Maybe Ident, (String, Expr))
  mkProp (name, e)
    | isAnonymousArgument e = do
      arg <- freshIdent'
      return (Just arg, (name, Var (Qualified Nothing arg)))
    | otherwise = return (Nothing, (name, e))
