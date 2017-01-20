-- |
-- This module implements the desugaring pass which replaces top-level binders with
-- case expressions.
--
module Language.PureScript.Sugar.CaseDeclarations
  ( desugarCases
  , desugarCasesModule
  ) where

import Prelude.Compat

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
-- Desugar case with pattern guards and pattern clauses to a
-- series of nested case expressions.
--
desugarCase :: (Monad m, MonadSupply m)
            => Expr
            -> m Expr
desugarCase (Case scrut alternatives) =
  let
    -- Alternatives which do not have guards are
    -- left as-is. Alternatives which
    --
    --   1) have multiple clauses of the form
    --      binder | g_1
    --             , g_2
    --             , ...
    --             , g_n
    --             -> expr
    --
    --   2) and/or contain pattern guards of the form
    --      binder | pat_bind <- e
    --             , ...
    --
    -- are desugared to a sequence of nested case expressions.
    --
    -- Consider an example case expression:
    --
    --   case e of
    --    (T s) | Just info <- Map.lookup s names
    --          , is_used info
    --          -> f info
    --
    -- We desugar this to
    --
    --   case e of
    --    (T s) -> case Map.lookup s names of
    --               Just info -> case is_used info of
    --                              True -> f info
    --                              (_    -> <partial>)
    --               (_ -> <partial>)
    --
    -- Note that if the original case is partial the desugared
    -- case is also partial.
    --
    -- Consider an exhaustive case expression:
    --
    --   case e of
    --    (T s) | Just info <- Map.lookup s names
    --          , is_used info
    --          -> f info
    --    _     -> Nothing
    --
    -- desugars to:
    --
    --    case e of
    --      _ -> let
    --                v _ = Nothing
    --           in
    --             case e of
    --                (T s) -> case Map.lookup s names of
    --                          Just info -> f info
    --                          _ -> v true
    --                _  -> v true
    --
    -- This might look strange but simplifies the algorithm a lot.
    --
    dsAlt :: (Monad m, MonadSupply m) => [CaseAlternative] -> m [CaseAlternative]
    dsAlt [] = pure []

    -- the trivial case: no guards
    dsAlt (a@(CaseAlternative _ [MkUnguarded _]) : as) =
      (a :) <$> dsAlt as

    -- we make sure to clean up all single conditional clauses
    -- as we would otherwise generate the out-of-line code for
    -- them which is unecessary.
    dsAlt (CaseAlternative ab ge : as)
      | not (null cond_guards) =
          (CaseAlternative ab cond_guards :)
            <$> dsGrdAlt ab rest as
      | otherwise = dsGrdAlt ab ge as
      where
        (cond_guards, rest) = span isSingleCondGuard ge

        isSingleCondGuard (GuardedExpr [ConditionGuard _] _) = True
        isSingleCondGuard _ = False

    dsGrdAlt :: (Monad m, MonadSupply m)
             => [Binder]
             -> [GuardedExpr]
             -> [CaseAlternative]
             -> m [CaseAlternative]
    dsGrdAlt bs [] rem_alts = pure rem_alts

    dsGrdAlt bs (GuardedExpr gs e : ge) rem_alts = do
      rhs <- dsGAltOutOfLine bs ge rem_alts $ \alt_fail ->
        let
          -- if the binder is a var binder we must not add
          -- the fail case as it results in unreachable
          -- alternative
          alt_fail' | all isVarBinder bs = []
                    | otherwise = alt_fail

        in Case scrut
            (CaseAlternative bs [MkUnguarded (dsGrd gs e alt_fail)]
              : alt_fail')

      return [ CaseAlternative [NullBinder] [MkUnguarded rhs]]
      where
        isVarBinder :: Binder -> Bool
        isVarBinder NullBinder = True
        isVarBinder (VarBinder _) = True
        isVarBinder (PositionedBinder _ _ b) = isVarBinder b
        isVarBinder (TypedBinder _ b) = isVarBinder b
        isVarBinder _ = False

    dsGrd :: [Guard] -> Expr -> [CaseAlternative] -> Expr
    dsGrd [] e _ = e
    dsGrd (ConditionGuard c : gs) e alt_fail
      | isTrueExpr c = dsGrd gs e alt_fail
      | otherwise =
        Case [c]
          (CaseAlternative [LiteralBinder (BooleanLiteral True)]
            [MkUnguarded (dsGrd gs e alt_fail)] : alt_fail)

    dsGrd (PatternGuard b g : gs) e alt_fail =
      Case [g]
        (CaseAlternative [b] [MkUnguarded (dsGrd gs e alt_fail)]
          : alt_fail)

    dsGAltOutOfLine :: (Monad m, MonadSupply m)
                    => [Binder]
                    -> [GuardedExpr]
                    -> [CaseAlternative]
                    -> ([CaseAlternative] -> Expr)
                    -> m Expr
    dsGAltOutOfLine alt_binder rem_guarded rem_alts mk_body
      | not (null rem_guarded) = do
        -- we still have some guarded expressions
        -- left to goto in case of guard failure
        rem_guarded' <- dsGrdAlt alt_binder rem_guarded rem_alts
        guard_fail <- freshIdent'
        let
          goto = Var (Qualified Nothing guard_fail) `App` Literal (BooleanLiteral True)
          alt_fail = [CaseAlternative [NullBinder] [MkUnguarded goto]]

        return $
          Let [ ValueDeclaration guard_fail Private [NullBinder]
                [MkUnguarded (Case scrut rem_guarded')]
              ] (mk_body alt_fail)

      | not (null rem_alts) = do
        -- there are some alternatives where we must
        -- go in case of guard failure
        rem_alts' <- dsAlt rem_alts
        guard_fail <- freshIdent'
        let
          goto = Var(Qualified Nothing guard_fail) `App` Literal (BooleanLiteral True)
          alt_fail = [CaseAlternative [NullBinder] [MkUnguarded goto]]

        return $
          Let [ ValueDeclaration guard_fail Private [NullBinder]
                [MkUnguarded (Case scrut rem_alts')]
              ] (mk_body alt_fail)

      | otherwise = do
        -- we have nowhere to go if a match fails. this is possibly
        -- a partial case expression.
        pure $ mk_body []

  in do
    alts' <- dsAlt alternatives
    pure $ case alts' of
      [CaseAlternative [NullBinder] [MkUnguarded v]]
        -> v
      _
        -> Case scrut alts'

desugarCase v = pure v

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
    desugarCase (Case vs alts)
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
    return $ Abs (Left ident) $ Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] [MkUnguarded val]]
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
          f' = mapM (\(GuardedExpr gs e) -> GuardedExpr gs <$> f e)
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
toDecls [ValueDeclaration ident nameKind bs [MkUnguarded val]] | all isVarBinder bs = do
  args <- mapM fromVarBinder bs
  let body = foldr (Abs . Left) val args
  guardWith (errorMessage (OverlappingArgNames (Just ident))) $ length (nub args) == length args
  return [ValueDeclaration ident nameKind [] [MkUnguarded body]]
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
toDecls ds@(ValueDeclaration ident _ bs (result : _) : _) = do
  let tuples = map toTuple ds

      isGuarded (MkUnguarded _) = False
      isGuarded _               = True

  unless (all ((== length bs) . length . fst) tuples) $
      throwError . errorMessage $ ArgListLengthsDiffer ident
  unless (not (null bs) || isGuarded result) $
      throwError . errorMessage $ DuplicateValueDeclaration ident
  caseDecl <- makeCaseDeclaration ident tuples
  return [caseDecl]
toDecls (PositionedDeclaration pos com d : ds) = do
  (d' : ds') <- rethrowWithPosition pos $ toDecls (d : ds)
  return (PositionedDeclaration pos com d' : ds')
toDecls ds = return ds

toTuple :: Declaration -> ([Binder], [GuardedExpr])
toTuple (ValueDeclaration _ _ bs result) = (bs, result)
toTuple (PositionedDeclaration _ _ d) = toTuple d
toTuple _ = internalError "Not a value declaration"

makeCaseDeclaration :: forall m. (MonadSupply m) => Ident -> [([Binder], [GuardedExpr])] -> m Declaration
makeCaseDeclaration ident alternatives = do
  let namedArgs = map findName . fst <$> alternatives
      argNames = foldl1 resolveNames namedArgs
  args <- if allUnique (catMaybes argNames)
            then mapM argName argNames
            else replicateM (length argNames) freshIdent'
  let vars = map (Var . Qualified Nothing) args
      binders = [ CaseAlternative bs result | (bs, result) <- alternatives ]
  case_ <- desugarCase (Case vars binders)
  let value = foldr (Abs . Left) case_ args

  return $ ValueDeclaration ident Public [] [MkUnguarded value]
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
