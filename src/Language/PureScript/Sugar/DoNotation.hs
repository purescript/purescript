-- | This module implements the desugaring pass which replaces do-notation statements with
-- appropriate calls to bind.

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.Sugar.DoNotation (desugarDoModule) where

import           Prelude.Compat

import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply.Class
import           Data.Monoid (First(..))
import           Language.PureScript.AST
import           Language.PureScript.Crash
import           Language.PureScript.Errors
import           Language.PureScript.Names
import qualified Language.PureScript.Constants as C

-- | Replace all @DoNotationBind@ and @DoNotationValue@ constructors with
-- applications of the bind function in scope, and all @DoNotationLet@
-- constructors with let expressions.
desugarDoModule :: forall m. (MonadSupply m, MonadError MultipleErrors m) => Module -> m Module
desugarDoModule (Module ss coms mn ds exts) = Module ss coms mn <$> parU ds desugarDo <*> pure exts

-- | Desugar a single do statement
desugarDo :: forall m. (MonadSupply m, MonadError MultipleErrors m) => Declaration -> m Declaration
desugarDo (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> rethrowWithPosition pos (desugarDo d)
desugarDo d =
  let (f, _, _) = everywhereOnValuesM return replace return
  in f d
  where
  bind :: Expr
  bind = Var (Qualified Nothing (Ident C.bind))

  discard :: Expr
  discard = Var (Qualified Nothing (Ident C.discard))

  replace :: Expr -> m Expr
  replace (Do els) = go els
  replace (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace v)
  replace other = return other

  go :: [DoNotationElement] -> m Expr
  go [] = internalError "The impossible happened in desugarDo"
  go [DoNotationValue val] = return val
  go (DoNotationValue val : rest) = do
    rest' <- go rest
    return $ App (App discard val) (Abs (Left (Ident C.__unused)) rest')
  go [DoNotationBind _ _] = throwError . errorMessage $ InvalidDoBind
  go (DoNotationBind NullBinder val : rest) = go (DoNotationValue val : rest)
  go (DoNotationBind b _ : _) | First (Just ident) <- foldMap fromIdent (binderNames b) =
      throwError . errorMessage $ CannotUseBindWithDo (Ident ident)
    where
      fromIdent (Ident i) | i `elem` [ C.bind, C.discard ] = First (Just i)
      fromIdent _ = mempty
  go (DoNotationBind (VarBinder ident) val : rest) = do
    rest' <- go rest
    return $ App (App bind val) (Abs (Left ident) rest')
  go (DoNotationBind binder val : rest) = do
    rest' <- go rest
    ident <- freshIdent'
    return $ App (App bind val) (Abs (Left ident) (Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] [MkUnguarded rest']]))
  go [DoNotationLet _] = throwError . errorMessage $ InvalidDoLet
  go (DoNotationLet ds : rest) = do
    let checkBind :: Declaration -> m ()
        checkBind (ValueDeclaration i@(Ident name) _ _ _)
          | name `elem` [ C.bind, C.discard ] = throwError . errorMessage $ CannotUseBindWithDo i
        checkBind (PositionedDeclaration pos _ decl) = rethrowWithPosition pos (checkBind decl)
        checkBind _ = pure ()
    mapM_ checkBind ds
    rest' <- go rest
    return $ Let ds rest'
  go (PositionedDoNotationElement pos com el : rest) = rethrowWithPosition pos $ PositionedValue pos com <$> go (el : rest)
