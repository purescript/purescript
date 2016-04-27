-- |
-- Optimizer step for simplifying "pass-through cases", for example:
--
-- > f (C x) = C x
-- > -- becomes
-- > f c@(C _) = c
--
-- This optimization is safe; PureScript provides no way to compare the memory
-- location of values, so data constructor calls can be optimized out freely.
--
-- This optimization must be performed after type checking, because it may
-- change the code in such a way that the result is ill-typed. This optimization
-- is performed before code generation, because it is common to all back-ends.
--

module Language.PureScript.CoreFn.Optimizer.PassThroughCases
  ( passThroughCases
  ) where

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Meta (Meta(IsConstructor))
import Language.PureScript.CoreFn.Module (Module, moduleDecls)
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Names

passThroughCases :: Module Ann -> Module Ann
passThroughCases m = m { moduleDecls = onBinds (moduleDecls m) }
  where
  onBinds :: [Bind Ann] -> [Bind Ann]
  onBinds = let (f, _, _) = everywhereOnValues id onExpr id
             in map f

  onExpr :: Expr Ann -> Expr Ann
  onExpr (Case ss ts cs) = Case ss ts (map optimize cs)
  onExpr e = e

  optimize :: CaseAlternative Ann -> CaseAlternative Ann
  optimize (CaseAlternative [bndr@(ConstructorBinder bndrAnn _ ctor prms)] (Right body))
    | isReconstruction ctor prms body =
        let (_, comments, type_, _) = bndrAnn
            varAnn = (Nothing, comments, type_, Nothing)
         in CaseAlternative [NamedBinder bndrAnn v bndr] (Right $ Var varAnn qv)
  optimize a = a

  isReconstruction :: Qualified (ProperName 'ConstructorName)
                   -> [Binder Ann]
                   -> Expr Ann
                   -> Bool
  isReconstruction ctor prms body =
    case dissectConstruction body of
      Just (ctor', args) ->
        Ident (runProperName $ disqualify ctor) == disqualify ctor'
        && all isBinderArg (prms `zip` args)
      Nothing -> False
    where isBinderArg (VarBinder _ i, Var _ i') = i == disqualify i'
          isBinderArg _ = False

  v :: Ident
  v = Ident "v"

  qv :: Qualified Ident
  qv = Qualified Nothing v


-- |
-- Return the constructor and arguments from what looks like a data constructor
-- call.
--
dissectConstruction :: Expr Ann -> Maybe (Qualified Ident, [Expr Ann])
dissectConstruction e =
  case unApp e of
    (Var ann c, args) | isConstructor ann -> Just (c, args)
    _ -> Nothing
  where
  isConstructor :: Ann -> Bool
  isConstructor (_, _, _, Just (IsConstructor _ _)) = True
  isConstructor _ = False
