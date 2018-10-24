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
desugarDo d =
  let ss = declSourceSpan d
      (f, _, _) = everywhereOnValuesM return (replace ss) return
  in rethrowWithPosition ss $ f d
  where
  bind :: SourceSpan -> Expr
  bind = flip Var (Qualified Nothing (Ident C.bind))

  discard :: SourceSpan -> Expr
  discard = flip Var (Qualified Nothing (Ident C.discard))

  replace :: SourceSpan -> Expr -> m Expr
  replace pos (Do els) = go pos els
  replace _ (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace pos v)
  replace _ other = return other

  go :: SourceSpan -> [DoNotationElement] -> m Expr
  go _ [] = internalError "The impossible happened in desugarDo"
  go _ [DoNotationValue val] = return val
  go pos (DoNotationValue val : rest) = do
    rest' <- go pos rest
    return $ App (App (discard pos) val) (Abs (VarBinder pos UnusedIdent) rest')
  go _ [DoNotationBind _ _] = throwError . errorMessage $ InvalidDoBind
  go _ (DoNotationBind b _ : _) | First (Just ident) <- foldMap fromIdent (binderNames b) =
      throwError . errorMessage $ CannotUseBindWithDo (Ident ident)
    where
      fromIdent (Ident i) | i `elem` [ C.bind, C.discard ] = First (Just i)
      fromIdent _ = mempty
  go pos (DoNotationBind (VarBinder ss ident) val : rest) = do
    rest' <- go pos rest
    return $ App (App (bind pos) val) (Abs (VarBinder ss ident) rest')
  go pos (DoNotationBind binder val : rest) = do
    rest' <- go pos rest
    ident <- freshIdent'
    return $ App (App (bind pos) val) (Abs (VarBinder pos ident) (Case [Var pos (Qualified Nothing ident)] [CaseAlternative [binder] [MkUnguarded rest']]))
  go _ [DoNotationLet _] = throwError . errorMessage $ InvalidDoLet
  go pos (DoNotationLet ds : rest) = do
    let checkBind :: Declaration -> m ()
        checkBind (ValueDecl (ss, _) i@(Ident name) _ _ _)
          | name `elem` [ C.bind, C.discard ] = throwError . errorMessage' ss $ CannotUseBindWithDo i
        checkBind _ = pure ()
    mapM_ checkBind ds
    rest' <- go pos rest
    return $ Let FromLet ds rest'
  go _ (PositionedDoNotationElement pos com el : rest) = rethrowWithPosition pos $ PositionedValue pos com <$> go pos (el : rest)
