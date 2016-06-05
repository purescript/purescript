-- |
-- This module implements the desugaring pass which replaces top-level binders with
-- case expressions.
--
module Language.PureScript.Sugar.CaseDeclarations
  ( desugarCases
  , desugarCasesModule
  ) where

import Prelude.Compat

import Data.Either (isLeft)
import Data.List (nub, groupBy, foldl1')
import Data.Maybe (catMaybes, mapMaybe)

import Control.Monad ((<=<), replicateM, join, unless)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.TypeChecker.Monad (guardWith)

-- |
-- Replace all top-level binders in a module with case expressions.
--
desugarCasesModule
  :: (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
desugarCasesModule (Module ss coms name ds exps) =
  rethrow (addHint (ErrorInModule name)) $
    Module ss coms name
      <$> (desugarCases <=< desugarAbs <=< validateCases $ ds)
      <*> pure exps

-- |
-- Validates that case head and binder lengths match.
--
validateCases :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
validateCases = flip parU f
  where
  (f, _, _) = everywhereOnValuesM return validate return

  validate :: Expr -> m Expr
  validate c@(Case vs alts) = do
    let l = length vs
        alts' = filter ((l /=) . length . caseAlternativeBinders) alts
    unless (null alts') $
      throwError . MultipleErrors $ fmap (altError l) (caseAlternativeBinders <$> alts')
    return c
  validate other = return other

  altError :: Int -> [Binder] -> ErrorMessage
  altError l bs = withPosition pos $ ErrorMessage [] $ CaseBinderLengthDiffers l bs
    where
    pos = foldl1' widenSpan (mapMaybe positionedBinder bs)

    widenSpan (SourceSpan n start end) (SourceSpan _ start' end') =
      SourceSpan n (min start start') (max end end')

    positionedBinder (PositionedBinder p _ _) = Just p
    positionedBinder _ = Nothing

desugarAbs :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
desugarAbs = flip parU f
  where
  (f, _, _) = everywhereOnValuesM return replace return

  replace :: Expr -> m Expr
  replace (Abs (Right binder) val) = do
    ident <- freshIdent'
    return $ Abs (Left ident) $ Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] (Right val)]
  replace other = return other

-- |
-- Replace all top-level binders with case expressions.
--
desugarCases :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
desugarCases = desugarRest <=< fmap join . flip parU toDecls . groupBy inSameGroup
  where
    desugarRest :: [Declaration] -> m [Declaration]
    desugarRest (TypeInstanceDeclaration name constraints className tys ds : rest) =
      (:) <$> (TypeInstanceDeclaration name constraints className tys <$> traverseTypeInstanceBody desugarCases ds) <*> desugarRest rest
    desugarRest (ValueDeclaration name nameKind bs result : rest) =
      let (_, f, _) = everywhereOnValuesTopDownM return go return
          f' (Left gs) = Left <$> mapM (pairM return f) gs
          f' (Right v) = Right <$> f v
      in (:) <$> (ValueDeclaration name nameKind bs <$> f' result) <*> desugarRest rest
      where
      go (Let ds val') = Let <$> desugarCases ds <*> pure val'
      go other = return other
    desugarRest (PositionedDeclaration pos com d : ds) = do
      (d' : ds') <- desugarRest (d : ds)
      return (PositionedDeclaration pos com d' : ds')
    desugarRest (d : ds) = (:) d <$> desugarRest ds
    desugarRest [] = pure []

inSameGroup :: Declaration -> Declaration -> Bool
inSameGroup (ValueDeclaration ident1 _ _ _) (ValueDeclaration ident2 _ _ _) = ident1 == ident2
inSameGroup (PositionedDeclaration _ _ d1) d2 = inSameGroup d1 d2
inSameGroup d1 (PositionedDeclaration _ _ d2) = inSameGroup d1 d2
inSameGroup _ _ = False

toDecls :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
toDecls [ValueDeclaration ident nameKind bs (Right val)] | all isVarBinder bs = do
  args <- mapM fromVarBinder bs
  let body = foldr (Abs . Left) val args
  guardWith (errorMessage (OverlappingArgNames (Just ident))) $ length (nub args) == length args
  return [ValueDeclaration ident nameKind [] (Right body)]
  where
  isVarBinder :: Binder -> Bool
  isVarBinder NullBinder = True
  isVarBinder (VarBinder _) = True
  isVarBinder (PositionedBinder _ _ b) = isVarBinder b
  isVarBinder (TypedBinder _ b) = isVarBinder b
  isVarBinder _ = False

  fromVarBinder :: Binder -> m Ident
  fromVarBinder NullBinder = freshIdent'
  fromVarBinder (VarBinder name) = return name
  fromVarBinder (PositionedBinder _ _ b) = fromVarBinder b
  fromVarBinder (TypedBinder _ b) = fromVarBinder b
  fromVarBinder _ = internalError "fromVarBinder: Invalid argument"
toDecls ds@(ValueDeclaration ident _ bs result : _) = do
  let tuples = map toTuple ds
  unless (all ((== length bs) . length . fst) tuples) $
      throwError . errorMessage $ ArgListLengthsDiffer ident
  unless (not (null bs) || isLeft result) $
      throwError . errorMessage $ DuplicateValueDeclaration ident
  caseDecl <- makeCaseDeclaration ident tuples
  return [caseDecl]
toDecls (PositionedDeclaration pos com d : ds) = do
  (d' : ds') <- rethrowWithPosition pos $ toDecls (d : ds)
  return (PositionedDeclaration pos com d' : ds')
toDecls ds = return ds

toTuple :: Declaration -> ([Binder], Either [(Guard, Expr)] Expr)
toTuple (ValueDeclaration _ _ bs result) = (bs, result)
toTuple (PositionedDeclaration _ _ d) = toTuple d
toTuple _ = internalError "Not a value declaration"

makeCaseDeclaration :: forall m. (MonadSupply m) => Ident -> [([Binder], Either [(Guard, Expr)] Expr)] -> m Declaration
makeCaseDeclaration ident alternatives = do
  let namedArgs = map findName . fst <$> alternatives
      argNames = foldl1 resolveNames namedArgs
  args <- if allUnique (catMaybes argNames)
            then mapM argName argNames
            else replicateM (length argNames) freshIdent'
  let vars = map (Var . Qualified Nothing) args
      binders = [ CaseAlternative bs result | (bs, result) <- alternatives ]
      value = foldr (Abs . Left) (Case vars binders) args
  return $ ValueDeclaration ident Public [] (Right value)
  where
  -- We will construct a table of potential names.
  -- VarBinders will become Just _ which is a potential name.
  -- Everything else becomes Nothing, which indicates that we
  -- have to generate a name.
  findName :: Binder -> Maybe Ident
  findName (VarBinder name) = Just name
  findName (PositionedBinder _ _ binder) = findName binder
  findName _ = Nothing

  -- We still have to make sure the generated names are unique, or else
  -- we will end up constructing an invalid function.
  allUnique :: (Eq a) => [a] -> Bool
  allUnique xs = length xs == length (nub xs)

  argName :: Maybe Ident -> m Ident
  argName (Just name) = return name
  argName _ = freshIdent'

  -- Combine two lists of potential names from two case alternatives
  -- by zipping correspoding columns.
  resolveNames :: [Maybe Ident] -> [Maybe Ident] -> [Maybe Ident]
  resolveNames = zipWith resolveName

  -- Resolve a pair of names. VarBinder beats NullBinder, and everything
  -- else results in Nothing.
  resolveName :: Maybe Ident -> Maybe Ident -> Maybe Ident
  resolveName (Just a) (Just b)
    | a == b = Just a
    | otherwise = Nothing
  resolveName _ _ = Nothing
