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
import Data.Maybe (catMaybes)

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
desugarCaseGuards declarations = parU declarations go
  where
    go d =
      let (f, _, _) = everywhereOnValuesM return desugarGuardedExprs return
      in f d

-- |
-- Desugar case with pattern guards and pattern clauses to a
-- series of nested case expressions.
--
desugarGuardedExprs
  :: forall m. (MonadSupply m)
  => Expr
  -> m Expr
desugarGuardedExprs (Case sa scrut alternatives)
  | any (not . isTrivialExpr) scrut = do
    -- in case the scrutinee is non trivial (e.g. not a Var or Literal)
    -- we may evaluate the scrutinee more than once when a guard occurrs.
    -- We bind the scrutinee to Vars here to mitigate this case.
    (scrut', scrut_decls) <- unzip <$> forM scrut (\e -> do
      scrut_id <- freshIdent'
      pure ( Var sa (Qualified Nothing scrut_id)
           , ValueDecl sa scrut_id Private [] [mkUnguarded e]
           )
      )
    Let sa scrut_decls <$> desugarGuardedExprs (Case sa scrut' alternatives)
  where
    isTrivialExpr Var{} = True
    isTrivialExpr Literal{} = True
    isTrivialExpr (Accessor _ _ e) = isTrivialExpr e
    isTrivialExpr (Parens _ e) = isTrivialExpr e
    isTrivialExpr (TypedValue _ _ e _) = isTrivialExpr e
    isTrivialExpr _ = False

desugarGuardedExprs (Case sa scrut alternatives) =
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
    desugarAlternatives [] = pure []

    -- the trivial case: no guards
    desugarAlternatives (a@(CaseAlternative _ _ [MkUnguarded _ _]) : as) =
      (a :) <$> desugarAlternatives as

    -- Special case: CoreFn understands single condition guards on
    -- binders right hand side.
    desugarAlternatives (CaseAlternative sa' ab ge : as)
      | not (null cond_guards) =
          (CaseAlternative sa' ab cond_guards :)
            <$> desugarGuardedAlternative ab rest as
      | otherwise = desugarGuardedAlternative ab ge as
      where
        (cond_guards, rest) = span isSingleCondGuard ge

        isSingleCondGuard (GuardedExpr _ [ConditionGuard _ _] _) = True
        isSingleCondGuard _ = False

    desugarGuardedAlternative :: [Binder]
                              -> [GuardedExpr]
                              -> [CaseAlternative]
                              -> m [CaseAlternative]
    desugarGuardedAlternative _vb [] rem_alts =
      desugarAlternatives rem_alts

    desugarGuardedAlternative vb (GuardedExpr ss gs e : ge) rem_alts = do
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
        in Case (ss, []) scrut
            (CaseAlternative ss vb [mkUnguarded (desugarGuard gs e alt_fail)]
              : alt_fail')

      return [ CaseAlternative ss scrut_nullbinder [mkUnguarded rhs]]

    desugarGuard :: [Guard] -> Expr -> [CaseAlternative] -> Expr
    desugarGuard [] e _ = e
    desugarGuard (ConditionGuard ss c : gs) e match_failed
      | isTrueExpr c = desugarGuard gs e match_failed
      | otherwise =
        Case (ss, []) [c]
          (CaseAlternative ss [LiteralBinder ss (BooleanLiteral True)]
            [mkUnguarded (desugarGuard gs e match_failed)] : match_failed)

    desugarGuard (PatternGuard ss vb g : gs) e match_failed =
      Case (ss, []) [g]
        (CaseAlternative ss [vb] [mkUnguarded (desugarGuard gs e match_failed)]
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
          goto_rem_case = App sa (Var sa (Qualified Nothing rem_case_id)) (Literal sa (BooleanLiteral True))
          alt_fail = [CaseAlternative (fst sa) [NullBinder (fst sa)] [mkUnguarded goto_rem_case]]

        pure $ Let sa [
          ValueDecl sa rem_case_id Private []
            [mkUnguarded (Abs sa (VarBinder (fst sa) unused_binder) desugared)]
          ] (mk_body alt_fail)

      | otherwise
      = pure $ mk_body []
      where
        mkCaseOfRemainingGuardsAndAlts
          | not (null rem_guarded)
          = Just $ Case sa scrut (CaseAlternative (fst sa) alt_binder rem_guarded : rem_alts)
          | not (null rem_alts)
          = Just $ Case sa scrut rem_alts
          | otherwise
          = Nothing

    scrut_nullbinder :: [Binder]
    scrut_nullbinder = replicate (length scrut) (NullBinder (fst sa))

    -- case expressions with a single alternative which have
    -- a NullBinder occur frequently after desugaring
    -- complex guards. This function removes these superflous
    -- cases.
    optimize :: Expr -> Expr
    optimize (Case _ _ [CaseAlternative _ vb [MkUnguarded _ v]])
      | all isNullBinder vb = v
      where
        isNullBinder NullBinder{} = True
        isNullBinder (TypedBinder _ _ b) = isNullBinder b
        isNullBinder _ = False
    optimize e = e
  in do
    alts' <- desugarAlternatives alternatives
    return $ optimize (Case sa scrut alts')

desugarGuardedExprs (TypedValue sa infered e ty) =
  TypedValue sa infered <$> desugarGuardedExprs e <*> pure ty

desugarGuardedExprs v = pure v

mkUnguarded :: Expr -> GuardedExpr
mkUnguarded e = MkUnguarded (exprSourceSpan e) e

-- |
-- Validates that case head and binder lengths match.
--
validateCases :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
validateCases = flip parU f
  where
  (f, _, _) = everywhereOnValuesM return validate return

  validate :: Expr -> m Expr
  validate c@(Case _ vs alts) = do
    let l = length vs
        alts' = filter ((l /=) . length . caseAlternativeBinders) alts
    unless (null alts') $
      throwError . MultipleErrors $ fmap (altError l) (caseAlternativeBinders <$> alts')
    return c
  validate other = return other

  altError :: Int -> [Binder] -> ErrorMessage
  altError l bs = withPosition pos $ ErrorMessage [] $ CaseBinderLengthDiffers l bs
    where
    pos = foldl1' widenSpan (map binderSourceSpan bs)

    widenSpan (SourceSpan n start end) (SourceSpan _ start' end') =
      SourceSpan n (min start start') (max end end')

desugarAbs :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
desugarAbs = flip parU f
  where
  (f, _, _) = everywhereOnValuesM return replace return

  replace :: Expr -> m Expr
  replace (Abs sa binder val) = do
    ident <- freshIdent'
    let
      binderSpan = binderSourceSpan binder
      exprSpan = exprSourceAnn val
    return
      $ Abs sa (VarBinder binderSpan ident)
      $ Case sa
        [Var (binderSpan, []) (Qualified Nothing ident)]
        [CaseAlternative (fst sa) [binder] [MkUnguarded (fst exprSpan) val]]
  replace other = return other

-- |
-- Replace all top-level binders with case expressions.
--
desugarCases :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
desugarCases = desugarRest <=< fmap join . flip parU toDecls . groupBy inSameGroup
  where
    desugarRest :: [Declaration] -> m [Declaration]
    desugarRest (TypeInstanceDeclaration sa name constraints className tys ds : rest) =
      (:) <$> (TypeInstanceDeclaration sa name constraints className tys <$> traverseTypeInstanceBody desugarCases ds) <*> desugarRest rest
    desugarRest (ValueDecl sa name nameKind bs result : rest) =
      let (_, f, _) = everywhereOnValuesTopDownM return go return
          f' = mapM (\(GuardedExpr ss gs e) -> GuardedExpr ss gs <$> f e)
      in (:) <$> (ValueDecl sa name nameKind bs <$> f' result) <*> desugarRest rest
      where
      go (Let ss ds val') = Let ss <$> desugarCases ds <*> pure val'
      go other = return other
    desugarRest (d : ds) = (:) d <$> desugarRest ds
    desugarRest [] = pure []

inSameGroup :: Declaration -> Declaration -> Bool
inSameGroup (ValueDeclaration vd1) (ValueDeclaration vd2) = valdeclIdent vd1 == valdeclIdent vd2
inSameGroup _ _ = False

toDecls :: forall m. (MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
toDecls [ValueDecl sa@(ss, _) ident nameKind bs [MkUnguarded _ val]] | all isIrrefutable bs = do
  args <- mapM fromVarBinder bs
  let body = foldr (Abs sa . uncurry VarBinder) val args
  guardWith (errorMessage' ss (OverlappingArgNames (Just ident))) $ length (ordNub args) == length args
  return [ValueDecl sa ident nameKind [] [mkUnguarded body]]
  where
  fromVarBinder :: Binder -> m (SourceSpan, Ident)
  fromVarBinder (NullBinder ss') = (,) ss' <$> freshIdent'
  fromVarBinder (VarBinder ss' name) = return (ss', name)
  fromVarBinder (TypedBinder _ _ b) = fromVarBinder b
  fromVarBinder _ = internalError "fromVarBinder: Invalid argument"
toDecls ds@(ValueDecl sa@(ss, _) ident _ bs (result : _) : _) = do
  let tuples = map toTuple ds

      isGuarded (MkUnguarded _ _) = False
      isGuarded _               = True

  unless (all ((== length bs) . length . getBinders) tuples) .
    throwError . errorMessage' ss $ ArgListLengthsDiffer ident
  unless (not (null bs) || isGuarded result) .
    throwError . errorMessage' ss $ DuplicateValueDeclaration ident
  caseDecl <- makeCaseDeclaration sa ident tuples
  return [caseDecl]
toDecls ds = return ds

toTuple :: Declaration -> (SourceAnn, [Binder], [GuardedExpr])
toTuple (ValueDecl ss _ _ bs result) = (ss, bs, result)
toTuple _ = internalError "Not a value declaration"

getBinders :: (a, [Binder], b) -> [Binder]
getBinders (_, bs, _) = bs

makeCaseDeclaration :: forall m. (MonadSupply m) => SourceAnn -> Ident -> [(SourceAnn, [Binder], [GuardedExpr])] -> m Declaration
makeCaseDeclaration sa@(ss, _) ident alternatives = do
  let namedArgs = map findName . getBinders <$> alternatives
      argNames = foldl1 resolveNames namedArgs
  args <- if allUnique (catMaybes (map snd argNames))
            then mapM argName argNames
            else zip (map fst argNames) <$> replicateM (length argNames) freshIdent'
  let vars = map (\(ss', i) -> Var (ss', []) (Qualified Nothing i)) args
      binders = [ CaseAlternative ss' bs result | ((ss', _), bs, result) <- alternatives ]
  let value = foldr (\(ss', i) -> Abs (ss', []) (VarBinder ss i)) (Case sa vars binders) args

  return $ ValueDecl (ss, []) ident Public [] [mkUnguarded value]
  where
  -- We will construct a table of potential names.
  -- VarBinders will become Just _ which is a potential name.
  -- Everything else becomes Nothing, which indicates that we
  -- have to generate a name.
  findName :: Binder -> (SourceSpan, Maybe Ident)
  findName (VarBinder ss' name) = (ss', Just name)
  findName b = (binderSourceSpan b, Nothing)

  -- We still have to make sure the generated names are unique, or else
  -- we will end up constructing an invalid function.
  allUnique :: (Ord a) => [a] -> Bool
  allUnique xs = length xs == length (ordNub xs)

  argName :: (SourceSpan, Maybe Ident) -> m (SourceSpan, Ident)
  argName (ss', Just name) = pure (ss', name)
  argName (ss', _) = (,) ss' <$> freshIdent'

  -- Combine two lists of potential names from two case alternatives
  -- by zipping correspoding columns.
  resolveNames :: [(SourceSpan, Maybe Ident)] -> [(SourceSpan, Maybe Ident)] -> [(SourceSpan, Maybe Ident)]
  resolveNames = zipWith resolveName

  -- Resolve a pair of names. VarBinder beats NullBinder, and everything
  -- else results in Nothing.
  resolveName :: (SourceSpan, Maybe Ident) -> (SourceSpan, Maybe Ident) -> (SourceSpan, Maybe Ident)
  resolveName (ss', Just a) (_, Just b)
    | a == b = (ss', Just a)
    | otherwise = (ss', Nothing)
  resolveName (ss', _) _ = (ss', Nothing)
