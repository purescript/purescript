-- |
-- This module implements the desugaring pass which replaces top-level binders with
-- case expressions.
--
module Language.PureScript.Sugar.CaseDeclarations
  ( desugarCases
  , desugarCasesModule
  , desugarCaseGuards
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Data.List (groupBy, foldl1')
import Data.Maybe (catMaybes, mapMaybe)

import Control.Monad ((<=<), forM, replicateM, join, unless)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
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

desugarCaseGuards
  :: forall m. (MonadSupply m, MonadError MultipleErrors m)
  => [Declaration]
  -> m [Declaration]
desugarCaseGuards declarations = parU declarations f
  where
    (f, _, _) = everywhereOnValuesM return desugarGuardedExprs return

-- |
-- Desugar case with pattern guards and pattern clauses to a
-- series of nested case expressions.
--
desugarGuardedExprs
  :: forall m. (MonadSupply m)
  => Expr
  -> m Expr
desugarGuardedExprs (Case scrut alternatives)
  | any (not . isTrivialExpr) scrut = do
    -- in case the scrutinee is non trivial (e.g. not a Var or Literal)
    -- we may evaluate the scrutinee more than once when a guard occurrs.
    -- We bind the scrutinee to Vars here to mitigate this case.
    (scrut', scrut_decls) <- unzip <$> forM scrut (\e -> do
      scrut_id <- freshIdent'
      pure ( Var (Qualified Nothing scrut_id)
           , ValueDeclaration scrut_id Private [] [MkUnguarded e]
           )
      )
    Let scrut_decls <$> desugarGuardedExprs (Case scrut' alternatives)
  where
    isTrivialExpr (Var _) = True
    isTrivialExpr (Literal _) = True
    isTrivialExpr (Accessor _ e) = isTrivialExpr e
    isTrivialExpr (Parens e) = isTrivialExpr e
    isTrivialExpr (PositionedValue _ _ e) = isTrivialExpr e
    isTrivialExpr (TypedValue _ e _) = isTrivialExpr e
    isTrivialExpr _ = False

desugarGuardedExprs (Case scrut alternatives) =
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
    desugarAlternatives :: [CaseAlternative]
                        -> m [CaseAlternative]
    desugarAlternatives [] = pure []

    -- the trivial case: no guards
    desugarAlternatives (a@(CaseAlternative _ [MkUnguarded _]) : as) =
      (a :) <$> desugarAlternatives as

    -- Special case: CoreFn understands single condition guards on
    -- binders right hand side.
    desugarAlternatives (CaseAlternative ab ge : as)
      | not (null cond_guards) =
          (CaseAlternative ab cond_guards :)
            <$> desugarGuardedAlternative ab rest as
      | otherwise = desugarGuardedAlternative ab ge as
      where
        (cond_guards, rest) = span isSingleCondGuard ge

        isSingleCondGuard (GuardedExpr [ConditionGuard _] _) = True
        isSingleCondGuard _ = False

    desugarGuardedAlternative :: [Binder]
                              -> [GuardedExpr]
                              -> [CaseAlternative]
                              -> m [CaseAlternative]
    desugarGuardedAlternative _vb [] rem_alts =
      desugarAlternatives rem_alts

    desugarGuardedAlternative vb (GuardedExpr gs e : ge) rem_alts = do
      rhs <- desugarAltOutOfLine vb ge rem_alts $ \alt_fail ->
        let
          -- if the binder is a var binder we must not add
          -- the fail case as it results in unreachable
          -- alternative
          alt_fail' | all isIrrefutable vb = []
                    | otherwise = alt_fail


          -- we are here:
          --
          -- case scrut of
          --   ...
          --   _ -> let
          --         v _ = <out of line case>
          --        in case scrut of -- we are here
          --            ...
          --
        in Case scrut
            (CaseAlternative vb [MkUnguarded (desugarGuard gs e alt_fail)]
              : alt_fail')

      return [ CaseAlternative scrut_nullbinder [MkUnguarded rhs]]

    desugarGuard :: [Guard] -> Expr -> [CaseAlternative] -> Expr
    desugarGuard [] e _ = e
    desugarGuard (ConditionGuard c : gs) e match_failed
      | isTrueExpr c = desugarGuard gs e match_failed
      | otherwise =
        Case [c]
          (CaseAlternative [LiteralBinder (BooleanLiteral True)]
            [MkUnguarded (desugarGuard gs e match_failed)] : match_failed)

    desugarGuard (PatternGuard vb g : gs) e match_failed =
      Case [g]
        (CaseAlternative [vb] [MkUnguarded (desugarGuard gs e match_failed)]
          : match_failed')
      where
        -- don't consider match_failed case if the binder is irrefutable
        match_failed' | isIrrefutable vb = []
                      | otherwise        = match_failed

    -- we generate a let-binding for the remaining guards
    -- and alternatives. A CaseAlternative is passed (or in
    -- fact the original case is partial non is passed) to
    -- mk_body which branches to the generated let-binding.
    desugarAltOutOfLine :: [Binder]
                        -> [GuardedExpr]
                        -> [CaseAlternative]
                        -> ([CaseAlternative] -> Expr)
                        -> m Expr
    desugarAltOutOfLine alt_binder rem_guarded rem_alts mk_body
      | Just rem_case <- mkCaseOfRemainingGuardsAndAlts = do

        desugared     <- desugarGuardedExprs rem_case
        rem_case_id   <- freshIdent'
        unused_binder <- freshIdent'

        let
          goto_rem_case :: Expr
          goto_rem_case = Var (Qualified Nothing rem_case_id)
            `App` Literal (BooleanLiteral True)
          alt_fail = [CaseAlternative [NullBinder] [MkUnguarded goto_rem_case]]

        pure $ Let [
          ValueDeclaration rem_case_id Private []
            [MkUnguarded (Abs (VarBinder unused_binder) desugared)]
          ] (mk_body alt_fail)

      | otherwise
      = pure $ mk_body []
      where
        mkCaseOfRemainingGuardsAndAlts
          | not (null rem_guarded)
          = Just $ Case scrut (CaseAlternative alt_binder rem_guarded : rem_alts)
          | not (null rem_alts)
          = Just $ Case scrut rem_alts
          | otherwise
          = Nothing

    scrut_nullbinder :: [Binder]
    scrut_nullbinder = replicate (length scrut) NullBinder

    -- case expressions with a single alternative which have
    -- a NullBinder occur frequently after desugaring
    -- complex guards. This function removes these superflous
    -- cases.
    optimize :: Expr -> Expr
    optimize (Case _ [CaseAlternative vb [MkUnguarded v]])
      | all isNullBinder vb = v
      where
        isNullBinder NullBinder = True
        isNullBinder (PositionedBinder _ _ b) = isNullBinder b
        isNullBinder (TypedBinder _ b) = isNullBinder b
        isNullBinder _ = False
    optimize e = e
  in do
    alts' <- desugarAlternatives alternatives
    return $ optimize (Case scrut alts')

desugarGuardedExprs (TypedValue infered e ty) =
  TypedValue infered <$> desugarGuardedExprs e <*> pure ty

desugarGuardedExprs (PositionedValue ss comms e) =
  PositionedValue ss comms <$> desugarGuardedExprs e

desugarGuardedExprs v = pure v

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
  replace (Abs (stripPositioned -> (VarBinder i)) val) =
    pure (Abs (VarBinder i) val)
  replace (Abs binder val) = do
    ident <- freshIdent'
    return $ Abs (VarBinder ident) $ Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] [MkUnguarded val]]
  replace other = return other

stripPositioned :: Binder -> Binder
stripPositioned (PositionedBinder _ _ binder) = stripPositioned binder
stripPositioned binder = binder

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
toDecls [ValueDeclaration ident nameKind bs [MkUnguarded val]] | all isIrrefutable bs = do
  args <- mapM fromVarBinder bs
  let body = foldr (Abs . VarBinder) val args
  guardWith (errorMessage (OverlappingArgNames (Just ident))) $ length (ordNub args) == length args
  return [ValueDeclaration ident nameKind [] [MkUnguarded body]]
  where
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
  let value = foldr (Abs . VarBinder) (Case vars binders) args

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
  allUnique :: (Ord a) => [a] -> Bool
  allUnique xs = length xs == length (ordNub xs)

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
