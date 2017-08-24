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
  let (f, _, _) = everywhereOnValuesM return replace return
  in rethrowWithPosition (declSourceSpan d) $ f d
  where
  bind :: SourceAnn -> Expr
  bind sa = Var sa (Qualified Nothing (Ident C.bind))

  discard :: SourceAnn -> Expr
  discard sa = Var sa (Qualified Nothing (Ident C.discard))

  replace :: Expr -> m Expr
  replace (Do _ els) = go els
  replace other = return other

  go :: [DoNotationElement] -> m Expr
  go [] = internalError "The impossible happened in desugarDo"
  go [DoNotationValue _ val] = return val
  go (DoNotationValue ss@(sa, _) val : rest) = do
    rest' <- go rest
    return $ App ss (App ss (discard sa) val) (Abs ss (VarBinder sa (Ident C.__unused)) rest')
  go [DoNotationBind (sa, _) _ _] = throwError . errorMessage' sa $ InvalidDoBind
  go (DoNotationBind (sa, _) b _ : _) | First (Just ident) <- foldMap fromIdent (binderNames b) =
      throwError . errorMessage' sa $ CannotUseBindWithDo (Ident ident)
    where
      fromIdent (Ident i) | i `elem` [ C.bind, C.discard ] = First (Just i)
      fromIdent _ = mempty
  go (DoNotationBind ss1 (VarBinder ss2 ident) val : rest) = do
    rest' <- go rest
    return $ App ss1 (App ss1 bind val) (Abs ss1 (VarBinder ss2 ident) rest')
  go (DoNotationBind ss@(sa, _) binder val : rest) = do
    rest' <- go rest
    ident <- freshIdent'
    return $
      App sa
        (App sa bind val)
        (Abs sa
          (VarBinder sa ident)
          (Case sa
            [Var sa (Qualified Nothing ident)]
            [CaseAlternative sa [binder] [MkUnguarded sa rest']]))
  go [DoNotationLet (sa, _) _] = throwError . errorMessage' sa $ InvalidDoLet
  go (DoNotationLet ss ds : rest) = do
    let checkBind :: Declaration -> m ()
        checkBind (ValueDecl (ss, _) i@(Ident name) _ _ _)
          | name `elem` [ C.bind, C.discard ] = throwError . errorMessage' ss $ CannotUseBindWithDo i
        checkBind _ = pure ()
    mapM_ checkBind ds
    rest' <- go rest
    return $ Let ss ds rest'
