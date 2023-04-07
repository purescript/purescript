-- | This module implements the desugaring pass which replaces do-notation statements with
-- appropriate calls to bind.

module Language.PureScript.Sugar.DoNotation (desugarDoModule) where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class (MonadSupply)
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import Language.PureScript.AST (Binder(..), CaseAlternative(..), Declaration, DoNotationElement(..), Expr(..), pattern MkUnguarded, Module(..), SourceSpan, pattern ValueDecl, WhereProvenance(..), binderNames, declSourceSpan, everywhereOnValuesM)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), errorMessage, errorMessage', parU, rethrowWithPosition)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, Qualified(..), byMaybeModuleName, freshIdent')
import Language.PureScript.Constants.Libs qualified as C

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
  bind :: SourceSpan -> Maybe ModuleName -> Expr
  bind ss m = Var ss (Qualified (byMaybeModuleName m) (Ident C.S_bind))

  discard :: SourceSpan -> Maybe ModuleName -> Expr
  discard ss m = Var ss (Qualified (byMaybeModuleName m) (Ident C.S_discard))

  replace :: SourceSpan -> Expr -> m Expr
  replace pos (Do m els) = go pos m els
  replace _ (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace pos v)
  replace _ other = return other

  stripPositionedBinder :: Binder -> (Maybe SourceSpan, Binder)
  stripPositionedBinder (PositionedBinder ss _ b) =
    let (ss', b') = stripPositionedBinder b
     in (ss' <|> Just ss, b')
  stripPositionedBinder b =
    (Nothing, b)

  go :: SourceSpan -> Maybe ModuleName -> [DoNotationElement] -> m Expr
  go _ _ [] = internalError "The impossible happened in desugarDo"
  go _ _ [DoNotationValue val] = return val
  go pos m (DoNotationValue val : rest) = do
    rest' <- go pos m rest
    return $ App (App (discard pos m) val) (Abs (VarBinder pos UnusedIdent) rest')
  go _ _ [DoNotationBind _ _] = throwError . errorMessage $ InvalidDoBind
  go _ _ (DoNotationBind b _ : _) | First (Just ident) <- foldMap fromIdent (binderNames b) =
      throwError . errorMessage $ CannotUseBindWithDo (Ident ident)
    where
      fromIdent (Ident i) | i `elem` [ C.S_bind, C.S_discard ] = First (Just i)
      fromIdent _ = mempty
  go pos m (DoNotationBind binder val : rest) = do
    rest' <- go pos m rest
    let (mss, binder') = stripPositionedBinder binder
    let ss = fromMaybe pos mss
    case binder' of
      NullBinder ->
        return $ App (App (bind pos m) val) (Abs (VarBinder ss UnusedIdent) rest')
      VarBinder _ ident ->
        return $ App (App (bind pos m) val) (Abs (VarBinder ss ident) rest')
      _ -> do
        ident <- freshIdent'
        return $ App (App (bind pos m) val) (Abs (VarBinder pos ident) (Case [Var pos (Qualified ByNullSourcePos ident)] [CaseAlternative [binder] [MkUnguarded rest']]))
  go _ _ [DoNotationLet _] = throwError . errorMessage $ InvalidDoLet
  go pos m (DoNotationLet ds : rest) = do
    let checkBind :: Declaration -> m ()
        checkBind (ValueDecl (ss, _) i@(Ident name) _ _ _)
          | name `elem` [ C.S_bind, C.S_discard ] = throwError . errorMessage' ss $ CannotUseBindWithDo i
        checkBind _ = pure ()
    mapM_ checkBind ds
    rest' <- go pos m rest
    return $ Let FromLet ds rest'
  go _ m (PositionedDoNotationElement pos com el : rest) = rethrowWithPosition pos $ PositionedValue pos com <$> go pos m (el : rest)
