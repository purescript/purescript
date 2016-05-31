module Language.PureScript.Sugar.ObjectWildcards
  ( desugarObjectConstructors
  ) where

import Prelude.Compat

import Control.Monad (forM)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Data.List (partition)
import Data.Maybe (catMaybes)

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names

desugarObjectConstructors
  :: forall m
   . (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
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
    , isAnonymousArgument u = do arg <- freshIdent'
                                 return $ Abs (Left arg) $ App (App op val) (Var (Qualified Nothing arg))
    | b' <- stripPositionInfo b
    , BinaryNoParens op u val <- b'
    , isAnonymousArgument u = do arg <- freshIdent'
                                 return $ Abs (Left arg) $ App (App op (Var (Qualified Nothing arg))) val
  desugarExpr (Literal (ObjectLiteral ps)) = wrapLambda (Literal . ObjectLiteral) ps
  desugarExpr (ObjectUpdate u ps) | isAnonymousArgument u = do
    obj <- freshIdent'
    Abs (Left obj) <$> wrapLambda (ObjectUpdate (argToExpr obj)) ps
  desugarExpr (ObjectUpdate obj ps) = wrapLambda (ObjectUpdate obj) ps
  desugarExpr (Accessor prop u) | isAnonymousArgument u = do
    arg <- freshIdent'
    return $ Abs (Left arg) (Accessor prop (argToExpr arg))
  desugarExpr (Case args cas) | any isAnonymousArgument args = do
    argIdents <- forM args freshIfAnon
    let args' = zipWith (\p -> maybe p argToExpr) args argIdents
    return $ foldr (Abs . Left) (Case args' cas) (catMaybes argIdents)
  desugarExpr (IfThenElse u t f) | any isAnonymousArgument [u, t, f] = do
    u' <- freshIfAnon u
    t' <- freshIfAnon t
    f' <- freshIfAnon f
    let if_ = IfThenElse (maybe u argToExpr u') (maybe t argToExpr t') (maybe f argToExpr f')
    return $ foldr (Abs . Left) if_ (catMaybes [u', t', f'])
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
  mkProp (name, e) = do
    arg <- freshIfAnon e
    return (arg, (name, maybe e argToExpr arg))

  freshIfAnon :: Expr -> m (Maybe Ident)
  freshIfAnon u
    | isAnonymousArgument u = Just <$> freshIdent'
    | otherwise = return Nothing

  argToExpr :: Ident -> Expr
  argToExpr = Var . Qualified Nothing
