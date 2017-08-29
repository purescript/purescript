module Language.PureScript.Sugar.ObjectWildcards
  ( desugarObjectConstructors
  , desugarDecl
  ) where

import           Prelude.Compat

import           Control.Monad (forM)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply.Class
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.Maybe (catMaybes)
import           Language.PureScript.AST
import           Language.PureScript.Environment (NameKind(..))
import           Language.PureScript.Errors
import           Language.PureScript.Names
import           Language.PureScript.PSString (PSString)


desugarObjectConstructors
  :: forall m
   . (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
desugarObjectConstructors (Module ss coms mn ds exts) = Module ss coms mn <$> mapM desugarDecl ds <*> pure exts

desugarDecl :: forall m. (MonadSupply m, MonadError MultipleErrors m) => Declaration -> m Declaration
desugarDecl d = rethrowWithPosition (declSourceSpan d) $ fn d
  where
  (fn, _, _) = everywhereOnValuesTopDownM return desugarExpr return

  desugarExpr :: Expr -> m Expr
  desugarExpr (AnonymousArgument (ss, _)) =
    throwError $ errorMessage' ss IncorrectAnonymousArgument
  desugarExpr (Parens sa b)
    | BinaryNoParens sa' op val u <- b
    , isAnonymousArgument u = do
        arg <- freshIdent'
        return $
          Abs sa (VarBinder (exprSourceSpan u) arg) $
            App sa' (App sa' op val) (Var (exprSourceAnn u) (Qualified Nothing arg))
  desugarExpr (Literal sa (ObjectLiteral ps)) = wrapLambdaAssoc (Literal sa . ObjectLiteral) ps
  desugarExpr (ObjectUpdateNested sa obj ps) = transformNestedUpdate sa obj ps
  desugarExpr (Accessor sa prop u)
    | Just props <- peelAnonAccessorChain u = do
      arg <- freshIdent'
      let argAnn = exprSourceAnn u
      return . Abs sa (VarBinder (fst argAnn) arg) $ foldr (Accessor sa) (argToExpr (argAnn, arg)) (prop:props)
  desugarExpr (Case sa args cas) | any isAnonymousArgument args = do
    argIdents <- forM args freshIfAnon
    let args' = zipWith (`maybe` argToExpr) args argIdents
    return $ foldr (\((ss, _), i) -> Abs sa (VarBinder ss i)) (Case sa args' cas) (catMaybes argIdents)
  desugarExpr (IfThenElse sa u t f) | any isAnonymousArgument [u, t, f] = do
    u' <- freshIfAnon u
    t' <- freshIfAnon t
    f' <- freshIfAnon f
    let if_ = IfThenElse sa (maybe u argToExpr u') (maybe t argToExpr t') (maybe f argToExpr f')
    return $ foldr (\(sa', i) -> Abs sa (VarBinder (fst sa') i)) if_ (catMaybes [u', t', f'])
  desugarExpr e = return e

  transformNestedUpdate :: SourceAnn -> Expr -> PathTree Expr -> m Expr
  transformNestedUpdate sa obj ps = do
    -- If we don't have an anonymous argument then we need to generate a let wrapper
    -- so that the object expression isn't re-evaluated for each nested update.
    val <- freshIdent'
    let valExpr = argToExpr (valAnn, val)
    if isAnonymousArgument obj
      then Abs sa (VarBinder valSpan val) <$> wrapLambda (buildUpdates valExpr) ps
      else wrapLambda (buildLet val . buildUpdates valExpr) ps
    where
      valAnn@(valSpan, _) = exprSourceAnn obj

      buildLet val = Let (declSourceSpan d, []) [ValueDecl (declSourceSpan d, []) val Public [] [MkUnguarded valSpan obj]]

      -- recursively build up the nested `ObjectUpdate` expressions
      buildUpdates :: Expr -> PathTree Expr -> Expr
      buildUpdates val (PathTree vs) = ObjectUpdate sa val (goLayer [] <$> runAssocList vs) where
        goLayer :: [PSString] -> (PSString, PathNode Expr) -> (PSString, Expr)
        goLayer _ (key, Leaf expr) = (key, expr)
        goLayer path (key, Branch (PathTree branch)) =
          let path' = path ++ [key]
              updates = goLayer path' <$> runAssocList branch
              accessor = foldl' (flip (Accessor (exprSourceAnn val))) val path'
          in (key, ObjectUpdate sa accessor updates)

  wrapLambda :: forall t. Traversable t => (t Expr -> Expr) -> t Expr -> m Expr
  wrapLambda mkVal ps = do
    args <- traverse processExpr ps
    return $ foldr (\(sa, i) -> Abs sa $ VarBinder (fst sa) i) (mkVal (snd <$> args)) (catMaybes $ toList (fst <$> args))
    where
      processExpr :: Expr -> m (Maybe (SourceAnn, Ident), Expr)
      processExpr e = do
        arg <- freshIfAnon e
        return (arg, maybe e argToExpr arg)

  wrapLambdaAssoc :: ([(PSString, Expr)] -> Expr) -> [(PSString, Expr)] -> m Expr
  wrapLambdaAssoc mkVal = wrapLambda (mkVal . runAssocList) . AssocList

  peelAnonAccessorChain :: Expr -> Maybe [PSString]
  peelAnonAccessorChain (Accessor _ p e) = (p :) <$> peelAnonAccessorChain e
  peelAnonAccessorChain AnonymousArgument{} = Just []
  peelAnonAccessorChain _ = Nothing

  isAnonymousArgument :: Expr -> Bool
  isAnonymousArgument AnonymousArgument{} = True
  isAnonymousArgument _ = False

  freshIfAnon :: Expr -> m (Maybe (SourceAnn, Ident))
  freshIfAnon u
    | isAnonymousArgument u = do
        i <- freshIdent'
        pure (Just (exprSourceAnn u, i))
    | otherwise = return Nothing

  argToExpr :: (SourceAnn, Ident) -> Expr
  argToExpr (sa, i) = Var sa (Qualified Nothing i)
