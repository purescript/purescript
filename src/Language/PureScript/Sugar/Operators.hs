{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
--
module Language.PureScript.Sugar.Operators (
  rebracket,
  removeSignedLiterals,
  desugarOperatorSections
) where

import Prelude ()
import Prelude.Compat

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Binders
import Language.PureScript.Sugar.Operators.Expr
import Language.PureScript.Traversals (defS)

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import qualified Language.PureScript.Constants as C

-- TODO: in 0.9 operators names can have their own type rather than being in a sum with `Ident`, and `AliasName` no longer needs to be optional

-- |
-- An operator associated with its declaration position, fixity, and the name
-- of the function or data constructor it is an alias for.
--
type FixityRecord = (Qualified Ident, SourceSpan, Fixity, Maybe AliasName)

-- |
-- An operator can be an alias for a function or a data constructor.
--
type AliasName = Either (Qualified Ident) (Qualified (ProperName 'ConstructorName))

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket
  :: forall m
   . (Applicative m, MonadError MultipleErrors m)
  => [ExternsFile]
  -> [Module]
  -> m [Module]
rebracket externs ms = do
  let fixities = concatMap externsFixities externs ++ concatMap collectFixities ms
  ensureNoDuplicates $ map (\(i, pos, _, _) -> (i, pos)) fixities
  let opTable = customOperatorTable $ map (\(i, _, f, _) -> (i, f)) fixities
  ms' <- traverse (rebracketModule opTable) ms
  let aliased = M.fromList (mapMaybe makeLookupEntry fixities)
  mapM (renameAliasedOperators aliased) ms'

  where

  makeLookupEntry :: FixityRecord -> Maybe (Qualified Ident, AliasName)
  makeLookupEntry (qname, _, _, alias) = (qname, ) <$> alias

  renameAliasedOperators :: M.Map (Qualified Ident) AliasName -> Module -> m Module
  renameAliasedOperators aliased (Module ss coms mn ds exts) =
    Module ss coms mn <$> mapM f' ds <*> pure exts
    where
    (f', _, _, _, _) = everywhereWithContextOnValuesM Nothing goDecl goExpr goBinder defS defS

    goDecl :: Maybe SourceSpan -> Declaration -> m (Maybe SourceSpan, Declaration)
    goDecl _ d@(PositionedDeclaration pos _ _) = return (Just pos, d)
    goDecl pos other = return (pos, other)

    goExpr :: Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
    goExpr _ e@(PositionedValue pos _ _) = return (Just pos, e)
    goExpr pos (Var name) = return (pos, case name `M.lookup` aliased of
      Just (Left alias) -> Var alias
      Just (Right alias) -> Constructor alias
      Nothing -> Var name)
    goExpr pos other = return (pos, other)

    goBinder :: Maybe SourceSpan -> Binder -> m (Maybe SourceSpan, Binder)
    goBinder _ b@(PositionedBinder pos _ _) = return (Just pos, b)
    goBinder pos (BinaryNoParensBinder (OpBinder name) lhs rhs) = case name `M.lookup` aliased of
      Just (Left alias) ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage $ InvalidOperatorInBinder (disqualify name) (disqualify alias)
      Just (Right alias) ->
        return (pos, ConstructorBinder alias [lhs, rhs])
      Nothing ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage $ UnknownValue name
    goBinder _ (BinaryNoParensBinder _ _ _) =
      internalError "BinaryNoParensBinder has no OpBinder"
    goBinder pos other = return (pos, other)

removeSignedLiterals :: Module -> Module
removeSignedLiterals (Module ss coms mn ds exts) = Module ss coms mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id

  go (UnaryMinus val) = App (Var (Qualified Nothing (Ident C.negate))) val
  go other = other

rebracketModule
  :: (Applicative m, MonadError MultipleErrors m)
  => [[(Qualified Ident, Associativity)]]
  -> Module
  -> m Module
rebracketModule opTable (Module ss coms mn ds exts) =
  let (f, _, _) = everywhereOnValuesTopDownM return (matchExprOperators opTable) (matchBinderOperators opTable)
  in Module ss coms mn <$> (map removeParens <$> parU ds f) <*> pure exts

removeParens :: Declaration -> Declaration
removeParens =
  let (f, _, _) = everywhereOnValues id goExpr goBinder
  in f
  where
  goExpr (Parens val) = val
  goExpr val = val
  goBinder (ParensInBinder b) = b
  goBinder b = b

externsFixities
  :: ExternsFile
  -> [FixityRecord]
externsFixities ExternsFile{..} =
   [ (Qualified (Just efModuleName) (Op op), internalModuleSourceSpan "", Fixity assoc prec, alias)
   | ExternsFixity assoc prec op alias <- efFixities
   ]

collectFixities :: Module -> [FixityRecord]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [FixityRecord]
  collect (PositionedDeclaration pos _ (FixityDeclaration fixity name alias)) =
    [(Qualified (Just moduleName) (Op name), pos, fixity, alias)]
  collect FixityDeclaration{} = internalError "Fixity without srcpos info"
  collect _ = []

ensureNoDuplicates
  :: MonadError MultipleErrors m
  => [(Qualified Ident, SourceSpan)]
  -> m ()
ensureNoDuplicates m = go $ sortBy (compare `on` fst) m
  where
  go [] = return ()
  go [_] = return ()
  go ((x@(Qualified (Just mn) name), _) : (y, pos) : _) | x == y =
    rethrow (addHint (ErrorInModule mn)) $
      rethrowWithPosition pos $
        throwError . errorMessage $ MultipleFixities name
  go (_ : rest) = go rest

customOperatorTable
  :: [(Qualified Ident, Fixity)]
  -> [[(Qualified Ident, Associativity)]]
customOperatorTable fixities =
  let
    userOps = map (\(name, Fixity a p) -> (name, p, a)) fixities
    sorted = sortBy (flip compare `on` (\(_, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, p, _) -> p)) sorted
  in
    map (map (\(name, _, a) -> (name, a))) groups

desugarOperatorSections
  :: forall m
   . (Applicative m, MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
desugarOperatorSections (Module ss coms mn ds exts) =
  Module ss coms mn <$> traverse goDecl ds <*> pure exts
  where

  goDecl :: Declaration -> m Declaration
  (goDecl, _, _) = everywhereOnValuesM return goExpr return

  goExpr :: Expr -> m Expr
  goExpr (OperatorSection op (Left val)) = return $ App op val
  goExpr (OperatorSection op (Right val)) = do
    arg <- freshIdent'
    return $ Abs (Left arg) $ App (App op (Var (Qualified Nothing arg))) val
  goExpr other = return other
