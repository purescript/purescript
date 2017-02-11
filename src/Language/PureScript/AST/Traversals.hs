-- |
-- AST traversal helpers
--
module Language.PureScript.AST.Traversals where

import Prelude.Compat

import Control.Monad

import Data.Foldable (fold)
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Literals
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.Types

guardedExprM :: Applicative m
             => (Guard a b -> m (Guard a b))
             -> (Expr a b -> m (Expr a b))
             -> GuardedExpr a b
             -> m (GuardedExpr a b)
guardedExprM f g (GuardedExpr guards rhs) =
  GuardedExpr <$> traverse f guards <*> g rhs

mapGuardedExpr :: (Guard a b -> Guard a b)
               -> (Expr a b -> Expr a b)
               -> GuardedExpr a b
               -> GuardedExpr a b
mapGuardedExpr f g (GuardedExpr guards rhs) =
  GuardedExpr (map f guards) (g rhs)

everywhereOnValues
  :: forall a b
   . (Declaration a b -> Declaration a b)
  -> (Expr a b -> Expr a b)
  -> (Binder a b -> Binder a b)
  -> ( Declaration a b -> Declaration a b
     , Expr a b -> Expr a b
     , Binder a b -> Binder a b
     )
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Declaration a b -> Declaration a b
  f' (DataBindingGroupDeclaration ds ann) = f (DataBindingGroupDeclaration (map f' ds) ann)
  f' (ValueDeclaration name nameKind bs val ann) = f (ValueDeclaration name nameKind (map h' bs) (map (mapGuardedExpr handleGuard g') val) ann)
  f' (BindingGroupDeclaration ds ann) = f (BindingGroupDeclaration (map (\(name, nameKind, val) -> (name, nameKind, g' val)) ds) ann)
  f' (TypeClassDeclaration name args implies deps ds ann) = f (TypeClassDeclaration name args implies deps (map f' ds) ann)
  f' (TypeInstanceDeclaration name cs className args ds ann) = f (TypeInstanceDeclaration name cs className args (mapTypeInstanceBody (map f') ds) ann)
  f' (PositionedDeclaration pos com d ann) = f (PositionedDeclaration pos com (f' d) ann)
  f' other = f other

  g' :: Expr a b -> Expr a b
  g' (Literal l ann) = g (Literal (lit g' l) ann)
  g' (UnaryMinus v ann) = g (UnaryMinus (g' v) ann)
  g' (BinaryNoParens op v1 v2 ann) = g (BinaryNoParens (g' op) (g' v1) (g' v2) ann)
  g' (Parens v ann) = g (Parens (g' v) ann)
  g' (TypeClassDictionaryConstructorApp name v ann) = g (TypeClassDictionaryConstructorApp name (g' v) ann)
  g' (Accessor prop v ann) = g (Accessor prop (g' v) ann)
  g' (ObjectUpdate obj vs ann) = g (ObjectUpdate (g' obj) (map (fmap g') vs) ann)
  g' (ObjectUpdateNested obj vs ann) = g (ObjectUpdateNested (g' obj) (fmap g' vs) ann)
  g' (Abs name v ann) = g (Abs name (g' v) ann)
  g' (App v1 v2 ann) = g (App (g' v1) (g' v2) ann)
  g' (IfThenElse v1 v2 v3 ann) = g (IfThenElse (g' v1) (g' v2) (g' v3) ann)
  g' (Case vs alts ann) = g (Case (map g' vs) (map handleCaseAlternative alts) ann)
  g' (TypedValue check v ty ann) = g (TypedValue check (g' v) ty ann)
  g' (Let ds v ann) = g (Let (map f' ds) (g' v) ann)
  g' (Do es ann) = g (Do (map handleDoNotationElement es) ann)
  g' (PositionedValue pos com v ann) = g (PositionedValue pos com (g' v) ann)
  g' other = g other

  h' :: Binder a b -> Binder a b
  h' (ConstructorBinder ctor bs ann) = h (ConstructorBinder ctor (map h' bs) ann)
  h' (BinaryNoParensBinder b1 b2 b3 ann) = h (BinaryNoParensBinder (h' b1) (h' b2) (h' b3) ann)
  h' (ParensInBinder b ann) = h (ParensInBinder (h' b) ann)
  h' (LiteralBinder l ann) = h (LiteralBinder (lit h' l) ann)
  h' (NamedBinder name b ann) = h (NamedBinder name (h' b) ann)
  h' (PositionedBinder pos com b ann) = h (PositionedBinder pos com (h' b) ann)
  h' (TypedBinder t b ann) = h (TypedBinder t (h' b) ann)
  h' other = h other

  lit :: (x -> x) -> Literal x -> Literal x
  lit go (ArrayLiteral as) = ArrayLiteral (map go as)
  lit go (ObjectLiteral as) = ObjectLiteral (map (fmap go) as)
  lit _ other = other

  handleCaseAlternative :: CaseAlternative a b -> CaseAlternative a b
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = map (mapGuardedExpr handleGuard g') (caseAlternativeResult ca)
       }

  handleDoNotationElement :: DoNotationElement a b -> DoNotationElement a b
  handleDoNotationElement (DoNotationValue v ann) = DoNotationValue (g' v) ann
  handleDoNotationElement (DoNotationBind b v ann) = DoNotationBind (h' b) (g' v) ann
  handleDoNotationElement (DoNotationLet ds ann) = DoNotationLet (map f' ds) ann
  handleDoNotationElement (PositionedDoNotationElement pos com e ann) = PositionedDoNotationElement pos com (handleDoNotationElement e) ann

  handleGuard :: Guard a b -> Guard a b
  handleGuard (ConditionGuard e) = ConditionGuard (g' e)
  handleGuard (PatternGuard b e) = PatternGuard (h' b) (g' e)

everywhereOnValuesTopDownM
  :: forall m a b
   . (Monad m)
  => (Declaration a b -> m (Declaration a b))
  -> (Expr a b -> m (Expr a b))
  -> (Binder a b -> m (Binder a b))
  -> ( Declaration a b -> m (Declaration a b)
     , Expr a b -> m (Expr a b)
     , Binder a b -> m (Binder a b)
     )
everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where

  f' :: Declaration a b -> m (Declaration a b)
  f' (DataBindingGroupDeclaration ds ann) = DataBindingGroupDeclaration <$> traverse (f' <=< f) ds <*> pure ann
  f' (ValueDeclaration name nameKind bs val ann) = ValueDeclaration name nameKind <$> traverse (h' <=< h) bs <*> traverse (guardedExprM handleGuard (g' <=< g)) val <*> pure ann
  f' (BindingGroupDeclaration ds ann) = BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds <*> pure ann
  f' (TypeClassDeclaration name args implies deps ds ann) = TypeClassDeclaration name args implies deps <$> traverse (f' <=< f) ds <*> pure ann
  f' (TypeInstanceDeclaration name cs className args ds ann) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds <*> pure ann
  f' (PositionedDeclaration pos com d ann) = PositionedDeclaration pos com <$> (f d >>= f') <*> pure ann
  f' other = f other

  g' :: Expr a b -> m (Expr a b)
  g' (Literal l ann) = Literal <$> lit (g >=> g') l <*> pure ann
  g' (UnaryMinus v ann) = UnaryMinus <$> (g v >>= g') <*> pure ann
  g' (BinaryNoParens op v1 v2 ann) = BinaryNoParens <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g') <*> pure ann
  g' (Parens v ann) = Parens <$> (g v >>= g') <*> pure ann
  g' (TypeClassDictionaryConstructorApp name v ann) = TypeClassDictionaryConstructorApp name <$> (g v >>= g') <*> pure ann
  g' (Accessor prop v ann) = Accessor prop <$> (g v >>= g') <*> pure ann
  g' (ObjectUpdate obj vs ann) = ObjectUpdate <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs <*> pure ann
  g' (ObjectUpdateNested obj vs ann) = ObjectUpdateNested <$> (g obj >>= g') <*> traverse (g' <=< g) vs <*> pure ann
  g' (Abs name v ann) = Abs name <$> (g v >>= g') <*> pure ann
  g' (App v1 v2 ann) = App <$> (g v1 >>= g') <*> (g v2 >>= g') <*> pure ann
  g' (IfThenElse v1 v2 v3 ann) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g') <*> pure ann
  g' (Case vs alts ann) = Case <$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts <*> pure ann
  g' (TypedValue check v ty ann) = TypedValue check <$> (g v >>= g') <*> pure ty <*> pure ann
  g' (Let ds v ann) = Let <$> traverse (f' <=< f) ds <*> (g v >>= g') <*> pure ann
  g' (Do es ann) = Do <$> traverse handleDoNotationElement es <*> pure ann
  g' (PositionedValue pos com v ann) = PositionedValue pos com <$> (g v >>= g') <*> pure ann
  g' other = g other

  h' :: Binder a b -> m (Binder a b)
  h' (LiteralBinder l ann) = LiteralBinder <$> lit (h >=> h') l <*> pure ann
  h' (ConstructorBinder ctor bs ann) = ConstructorBinder ctor <$> traverse (h' <=< h) bs <*> pure ann
  h' (BinaryNoParensBinder b1 b2 b3 ann) = BinaryNoParensBinder <$> (h b1 >>= h') <*> (h b2 >>= h') <*> (h b3 >>= h') <*> pure ann
  h' (ParensInBinder b ann) = ParensInBinder <$> (h b >>= h') <*> pure ann
  h' (NamedBinder name b ann) = NamedBinder name <$> (h b >>= h') <*> pure ann
  h' (PositionedBinder pos com b ann) = PositionedBinder pos com <$> (h b >>= h') <*> pure ann
  h' (TypedBinder t b ann) = TypedBinder t <$> (h b >>= h') <*> pure ann
  h' other = h other

  lit :: (x -> m x) -> Literal x -> m (Literal x)
  lit go (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM go) as
  lit go (ArrayLiteral as) = ArrayLiteral <$> traverse go as
  lit _ other = pure other

  handleCaseAlternative :: CaseAlternative a b -> m (CaseAlternative a b)
  handleCaseAlternative (CaseAlternative bs val) =
    CaseAlternative
      <$> traverse (h' <=< h) bs
      <*> traverse (guardedExprM handleGuard (g' <=< g)) val

  handleDoNotationElement :: DoNotationElement a b -> m (DoNotationElement a b)
  handleDoNotationElement (DoNotationValue v ann) = DoNotationValue <$> (g' <=< g) v <*> pure ann
  handleDoNotationElement (DoNotationBind b v ann) = DoNotationBind <$> (h' <=< h) b <*> (g' <=< g) v <*> pure ann
  handleDoNotationElement (DoNotationLet ds ann) = DoNotationLet <$> traverse (f' <=< f) ds <*> pure ann
  handleDoNotationElement (PositionedDoNotationElement pos com e ann) = PositionedDoNotationElement pos com <$> handleDoNotationElement e <*> pure ann

  handleGuard :: Guard a b -> m (Guard a b)
  handleGuard (ConditionGuard e) = ConditionGuard <$> (g' <=< g) e
  handleGuard (PatternGuard b e) = PatternGuard <$> (h' <=< h) b <*> (g' <=< g) e

everywhereOnValuesM
  :: forall m a b
   . (Monad m)
  => (Declaration a b -> m (Declaration a b))
  -> (Expr a b -> m (Expr a b))
  -> (Binder a b -> m (Binder a b))
  -> ( Declaration a b -> m (Declaration a b)
     , Expr a b -> m (Expr a b)
     , Binder a b -> m (Binder a b)
     )
everywhereOnValuesM f g h = (f', g', h')
  where

  f' :: Declaration a b -> m (Declaration a b)
  f' (DataBindingGroupDeclaration ds ann) = (DataBindingGroupDeclaration <$> traverse f' ds <*> pure ann) >>= f
  f' (ValueDeclaration name nameKind bs val ann) = (ValueDeclaration name nameKind <$> traverse h' bs <*> traverse (guardedExprM handleGuard g') val <*> pure ann) >>= f
  f' (BindingGroupDeclaration ds ann) = (BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds <*> pure ann) >>= f
  f' (TypeClassDeclaration name args implies deps ds ann) = (TypeClassDeclaration name args implies deps <$> traverse f' ds <*> pure ann) >>= f
  f' (TypeInstanceDeclaration name cs className args ds ann) = (TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse f') ds <*> pure ann) >>= f
  f' (PositionedDeclaration pos com d ann) = (PositionedDeclaration pos com <$> f' d <*> pure ann) >>= f
  f' other = f other

  g' :: Expr a b -> m (Expr a b)
  g' (Literal l ann) = (Literal <$> lit g' l <*> pure ann) >>= g
  g' (UnaryMinus v ann) = (UnaryMinus <$> g' v <*> pure ann) >>= g
  g' (BinaryNoParens op v1 v2 ann) = (BinaryNoParens <$> g' op <*> g' v1 <*> g' v2 <*> pure ann) >>= g
  g' (Parens v ann) = (Parens <$> g' v <*> pure ann) >>= g
  g' (TypeClassDictionaryConstructorApp name v ann) = (TypeClassDictionaryConstructorApp name <$> g' v <*> pure ann) >>= g
  g' (Accessor prop v ann) = (Accessor prop <$> g' v <*> pure ann) >>= g
  g' (ObjectUpdate obj vs ann) = (ObjectUpdate <$> g' obj <*> traverse (sndM g') vs <*> pure ann) >>= g
  g' (ObjectUpdateNested obj vs ann) = (ObjectUpdateNested <$> g' obj <*> traverse g' vs <*> pure ann) >>= g
  g' (Abs name v ann) = (Abs name <$> g' v <*> pure ann) >>= g
  g' (App v1 v2 ann) = (App <$> g' v1 <*> g' v2 <*> pure ann) >>= g
  g' (IfThenElse v1 v2 v3 ann) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3 <*> pure ann) >>= g
  g' (Case vs alts ann) = (Case <$> traverse g' vs <*> traverse handleCaseAlternative alts <*> pure ann) >>= g
  g' (TypedValue check v ty ann) = (TypedValue check <$> g' v <*> pure ty <*> pure ann) >>= g
  g' (Let ds v ann) = (Let <$> traverse f' ds <*> g' v <*> pure ann) >>= g
  g' (Do es ann) = (Do <$> traverse handleDoNotationElement es <*> pure ann) >>= g
  g' (PositionedValue pos com v ann) = (PositionedValue pos com <$> g' v <*> pure ann) >>= g
  g' other = g other

  h' :: Binder a b -> m (Binder a b)
  h' (LiteralBinder l ann) = (LiteralBinder <$> lit h' l <*> pure ann) >>= h
  h' (ConstructorBinder ctor bs ann) = (ConstructorBinder ctor <$> traverse h' bs <*> pure ann) >>= h
  h' (BinaryNoParensBinder b1 b2 b3 ann) = (BinaryNoParensBinder <$> h' b1 <*> h' b2 <*> h' b3 <*> pure ann) >>= h
  h' (ParensInBinder b ann) = (ParensInBinder <$> h' b <*> pure ann) >>= h
  h' (NamedBinder name b ann) = (NamedBinder name <$> h' b <*> pure ann) >>= h
  h' (PositionedBinder pos com b ann) = (PositionedBinder pos com <$> h' b <*> pure ann) >>= h
  h' (TypedBinder t b ann) = (TypedBinder t <$> h' b <*> pure ann) >>= h
  h' other = h other

  lit :: (x -> m x) -> Literal x -> m (Literal x)
  lit go (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM go) as
  lit go (ArrayLiteral as) = ArrayLiteral <$> traverse go as
  lit _ other = pure other

  handleCaseAlternative :: CaseAlternative a b -> m (CaseAlternative a b)
  handleCaseAlternative (CaseAlternative bs val) =
    CaseAlternative
      <$> traverse h' bs
      <*> traverse (guardedExprM handleGuard g') val

  handleDoNotationElement :: DoNotationElement a b -> m (DoNotationElement a b)
  handleDoNotationElement (DoNotationValue v ann) = DoNotationValue <$> g' v <*> pure ann
  handleDoNotationElement (DoNotationBind b v ann) = DoNotationBind <$> h' b <*> g' v <*> pure ann
  handleDoNotationElement (DoNotationLet ds ann) = DoNotationLet <$> traverse f' ds <*> pure ann
  handleDoNotationElement (PositionedDoNotationElement pos com e ann) = PositionedDoNotationElement pos com <$> handleDoNotationElement e <*> pure ann

  handleGuard :: Guard a b -> m (Guard a b)
  handleGuard (ConditionGuard e) = ConditionGuard <$> g' e
  handleGuard (PatternGuard b e) = PatternGuard <$> h' b <*> g' e

everythingOnValues
  :: forall r a b
   . (r -> r -> r)
  -> (Declaration a b -> r)
  -> (Expr a b -> r)
  -> (Binder a b -> r)
  -> (CaseAlternative a b -> r)
  -> (DoNotationElement a b -> r)
  -> ( Declaration a b -> r
     , Expr a b -> r
     , Binder a b -> r
     , CaseAlternative a b -> r
     , DoNotationElement a b -> r
     )
everythingOnValues (<>) f g h i j = (f', g', h', i', j')
  where

  f' :: Declaration a b -> r
  f' d@(DataBindingGroupDeclaration ds _) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ bs val _) = foldl (<>) (f d) (map h' bs ++ concatMap (\(GuardedExpr grd v) -> map k' grd ++ [g' v]) val)
  f' d@(BindingGroupDeclaration ds _) = foldl (<>) (f d) (map (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ _ ds _) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds) _) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ _ d1 _) = f d <> f' d1
  f' d = f d

  g' :: Expr a b -> r
  g' v@(Literal l _) = lit (g v) g' l
  g' v@(UnaryMinus v1 _) = g v <> g' v1
  g' v@(BinaryNoParens op v1 v2 _) = g v <> g' op <> g' v1 <> g' v2
  g' v@(Parens v1 _) = g v <> g' v1
  g' v@(TypeClassDictionaryConstructorApp _ v1 _) = g v <> g' v1
  g' v@(Accessor _ v1 _) = g v <> g' v1
  g' v@(ObjectUpdate obj vs _) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(ObjectUpdateNested obj vs _) = foldl (<>) (g v <> g' obj) (fmap g' vs)
  g' v@(Abs _ v1 _) = g v <> g' v1
  g' v@(App v1 v2 _) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse v1 v2 v3 _) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case vs alts _) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _ _) = g v <> g' v1
  g' v@(Let ds v1 _) = foldl (<>) (g v) (map f' ds) <> g' v1
  g' v@(Do es _) = foldl (<>) (g v) (map j' es)
  g' v@(PositionedValue _ _ v1 _) = g v <> g' v1
  g' v = g v

  h' :: Binder a b -> r
  h' b@(LiteralBinder l _) = lit (h b) h' l
  h' b@(ConstructorBinder _ bs _) = foldl (<>) (h b) (map h' bs)
  h' b@(BinaryNoParensBinder b1 b2 b3 _) = h b <> h' b1 <> h' b2 <> h' b3
  h' b@(ParensInBinder b1 _) = h b <> h' b1
  h' b@(NamedBinder _ b1 _) = h b <> h' b1
  h' b@(PositionedBinder _ _ b1 _) = h b <> h' b1
  h' b@(TypedBinder _ b1 _) = h b <> h' b1
  h' b = h b

  lit :: r -> (x -> r) -> Literal x -> r
  lit r go (ArrayLiteral as) = foldl (<>) r (map go as)
  lit r go (ObjectLiteral as) = foldl (<>) r (map (go . snd) as)
  lit r _ _ = r

  i' :: CaseAlternative a b -> r
  i' ca@(CaseAlternative bs gs) =
    foldl (<>) (i ca) (map h' bs ++ concatMap (\(GuardedExpr grd val) -> map k' grd ++ [g' val]) gs)

  j' :: DoNotationElement a b -> r
  j' e@(DoNotationValue v _) = j e <> g' v
  j' e@(DoNotationBind b v _) = j e <> h' b <> g' v
  j' e@(DoNotationLet ds _) = foldl (<>) (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ _ e1 _) = j e <> j' e1

  k' :: Guard a b -> r
  k' (ConditionGuard e) = g' e
  k' (PatternGuard b e) = h' b <> g' e

everythingWithContextOnValues
  :: forall s r a b
   . s
  -> r
  -> (r -> r -> r)
  -> (s -> Declaration a b       -> (s, r))
  -> (s -> Expr a b              -> (s, r))
  -> (s -> Binder a b            -> (s, r))
  -> (s -> CaseAlternative a b   -> (s, r))
  -> (s -> DoNotationElement a b -> (s, r))
  -> ( Declaration a b       -> r
     , Expr a b              -> r
     , Binder a b            -> r
     , CaseAlternative a b   -> r
     , DoNotationElement a b -> r)
everythingWithContextOnValues s0 r0 (<>) f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where

  f'' :: s -> Declaration a b -> r
  f'' s d = let (s', r) = f s d in r <> f' s' d

  f' :: s -> Declaration a b -> r
  f' s (DataBindingGroupDeclaration ds _) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ bs val _) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(GuardedExpr grd v) -> map (k' s) grd ++ [g'' s v]) val)
  f' s (BindingGroupDeclaration ds _) = foldl (<>) r0 (map (\(_, _, val) -> g'' s val) ds)
  f' s (TypeClassDeclaration _ _ _ _ ds _) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds) _) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ _ d1 _) = f'' s d1
  f' _ _ = r0

  g'' :: s -> Expr a b -> r
  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' :: s -> Expr a b -> r
  g' s (Literal l _) = lit g'' s l
  g' s (UnaryMinus v1 _) = g'' s v1
  g' s (BinaryNoParens op v1 v2 _) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1 _) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ v1 _) = g'' s v1
  g' s (Accessor _ v1 _) = g'' s v1
  g' s (ObjectUpdate obj vs _) = foldl (<>) (g'' s obj) (map (g'' s . snd) vs)
  g' s (ObjectUpdateNested obj vs _) = foldl (<>) (g'' s obj) (fmap (g'' s) vs)
  g' s (Abs _ v1 _) = g'' s v1
  g' s (App v1 v2 _) = g'' s v1 <> g'' s v2
  g' s (IfThenElse v1 v2 v3 _) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts _) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _ _) = g'' s v1
  g' s (Let ds v1 _) = foldl (<>) r0 (map (f'' s) ds) <> g'' s v1
  g' s (Do es _) = foldl (<>) r0 (map (j'' s) es)
  g' s (PositionedValue _ _ v1 _) = g'' s v1
  g' _ _ = r0

  h'' :: s -> Binder a b -> r
  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' :: s -> Binder a b -> r
  h' s (LiteralBinder l _) = lit h'' s l
  h' s (ConstructorBinder _ bs _) = foldl (<>) r0 (map (h'' s) bs)
  h' s (BinaryNoParensBinder b1 b2 b3 _) = h'' s b1 <> h'' s b2 <> h'' s b3
  h' s (ParensInBinder b _) = h'' s b
  h' s (NamedBinder _ b1 _) = h'' s b1
  h' s (PositionedBinder _ _ b1 _) = h'' s b1
  h' s (TypedBinder _ b1 _) = h'' s b1
  h' _ _ = r0

  lit :: (s -> x -> r) -> s -> Literal x -> r
  lit go s (ArrayLiteral as) = foldl (<>) r0 (map (go s) as)
  lit go s (ObjectLiteral as) = foldl (<>) r0 (map (go s . snd) as)
  lit _ _ _ = r0

  i'' :: s -> CaseAlternative a b -> r
  i'' s ca = let (s', r) = i s ca in r <> i' s' ca

  i' :: s -> CaseAlternative a b -> r
  i' s (CaseAlternative bs gs) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(GuardedExpr grd val) -> map (k' s) grd ++ [g'' s val]) gs)

  j'' :: s -> DoNotationElement a b -> r
  j'' s e = let (s', r) = j s e in r <> j' s' e

  j' :: s -> DoNotationElement a b -> r
  j' s (DoNotationValue v _) = g'' s v
  j' s (DoNotationBind b v _) = h'' s b <> g'' s v
  j' s (DoNotationLet ds _) = foldl (<>) r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ _ e1 _) = j'' s e1

  k' :: s -> Guard a b -> r
  k' s (ConditionGuard e) = g'' s e
  k' s (PatternGuard b e) = h'' s b <> g'' s e

everywhereWithContextOnValuesM
  :: forall m s a b
   . (Monad m)
  => s
  -> (s -> Declaration a b       -> m (s, Declaration a b))
  -> (s -> Expr a b              -> m (s, Expr a b))
  -> (s -> Binder a b            -> m (s, Binder a b))
  -> (s -> CaseAlternative a b   -> m (s, CaseAlternative a b))
  -> (s -> DoNotationElement a b -> m (s, DoNotationElement a b))
  -> ( Declaration a b       -> m (Declaration a b)
     , Expr a b              -> m (Expr a b)
     , Binder a b            -> m (Binder a b)
     , CaseAlternative a b   -> m (CaseAlternative a b)
     , DoNotationElement a b -> m (DoNotationElement a b)
     )
everywhereWithContextOnValuesM s0 f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds ann) = DataBindingGroupDeclaration <$> traverse (f'' s) ds <*> pure ann
  f' s (ValueDeclaration name nameKind bs val ann) = ValueDeclaration name nameKind <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val <*> pure ann
  f' s (BindingGroupDeclaration ds ann) = BindingGroupDeclaration <$> traverse (thirdM (g'' s)) ds <*> pure ann
  f' s (TypeClassDeclaration name args implies deps ds ann) = TypeClassDeclaration name args implies deps <$> traverse (f'' s) ds <*> pure ann
  f' s (TypeInstanceDeclaration name cs className args ds ann) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f'' s)) ds <*> pure ann
  f' s (PositionedDeclaration pos com d1 ann) = PositionedDeclaration pos com <$> f'' s d1 <*> pure ann
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (Literal l ann) = Literal <$> lit g'' s l <*> pure ann
  g' s (UnaryMinus v ann) = UnaryMinus <$> g'' s v <*> pure ann
  g' s (BinaryNoParens op v1 v2 ann) = BinaryNoParens <$> g'' s op <*> g'' s v1 <*> g'' s v2 <*> pure ann
  g' s (Parens v ann) = Parens <$> g'' s v <*> pure ann
  g' s (TypeClassDictionaryConstructorApp name v ann) = TypeClassDictionaryConstructorApp name <$> g'' s v <*> pure ann
  g' s (Accessor prop v ann) = Accessor prop <$> g'' s v <*> pure ann
  g' s (ObjectUpdate obj vs ann) = ObjectUpdate <$> g'' s obj <*> traverse (sndM (g'' s)) vs <*> pure ann
  g' s (ObjectUpdateNested obj vs ann) = ObjectUpdateNested <$> g'' s obj <*> traverse (g'' s) vs <*> pure ann
  g' s (Abs name v ann) = Abs name <$> g'' s v <*> pure ann
  g' s (App v1 v2 ann) = App <$> g'' s v1 <*> g'' s v2 <*> pure ann
  g' s (IfThenElse v1 v2 v3 ann) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3 <*> pure ann
  g' s (Case vs alts ann) = Case <$> traverse (g'' s) vs <*> traverse (i'' s) alts <*> pure ann
  g' s (TypedValue check v ty ann) = TypedValue check <$> g'' s v <*> pure ty <*> pure ann
  g' s (Let ds v ann) = Let <$> traverse (f'' s) ds <*> g'' s v <*> pure ann
  g' s (Do es ann) = Do <$> traverse (j'' s) es <*> pure ann
  g' s (PositionedValue pos com v ann) = PositionedValue pos com <$> g'' s v <*> pure ann
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (LiteralBinder l ann) = LiteralBinder <$> lit h'' s l <*> pure ann
  h' s (ConstructorBinder ctor bs ann) = ConstructorBinder ctor <$> traverse (h'' s) bs <*> pure ann
  h' s (BinaryNoParensBinder b1 b2 b3 ann) = BinaryNoParensBinder <$> h'' s b1 <*> h'' s b2 <*> h'' s b3 <*> pure ann
  h' s (ParensInBinder b ann) = ParensInBinder <$> h'' s b <*> pure ann
  h' s (NamedBinder name b ann) = NamedBinder name <$> h'' s b <*> pure ann
  h' s (PositionedBinder pos com b ann) = PositionedBinder pos com <$> h'' s b <*> pure ann
  h' s (TypedBinder t b ann) = TypedBinder t <$> h'' s b <*> pure ann
  h' _ other = return other

  lit :: (s -> x -> m x) -> s -> Literal x -> m (Literal x)
  lit go s (ArrayLiteral as) = ArrayLiteral <$> traverse (go s) as
  lit go s (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM (go s)) as
  lit _ _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative bs val) = CaseAlternative <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue v ann) = DoNotationValue <$> g'' s v <*> pure ann
  j' s (DoNotationBind b v ann) = DoNotationBind <$> h'' s b <*> g'' s v <*> pure ann
  j' s (DoNotationLet ds ann) = DoNotationLet <$> traverse (f'' s) ds <*> pure ann
  j' s (PositionedDoNotationElement pos com e1 ann) = PositionedDoNotationElement pos com <$> j'' s e1 <*> pure ann

  k' s (ConditionGuard e) = ConditionGuard <$> g'' s e
  k' s (PatternGuard b e) = PatternGuard <$> h'' s b <*> g'' s e

everythingWithScope
  :: forall r a b
   . (Monoid r)
  => (S.Set Ident -> Declaration a b -> r)
  -> (S.Set Ident -> Expr a b -> r)
  -> (S.Set Ident -> Binder a b -> r)
  -> (S.Set Ident -> CaseAlternative a b -> r)
  -> (S.Set Ident -> DoNotationElement a b -> r)
  -> ( S.Set Ident -> Declaration a b -> r
     , S.Set Ident -> Expr a b -> r
     , S.Set Ident -> Binder a b -> r
     , S.Set Ident -> CaseAlternative a b -> r
     , S.Set Ident -> DoNotationElement a b -> r
     )
everythingWithScope f g h i j = (f'', g'', h'', i'', \s -> snd . j'' s)
  where
  -- Avoid importing Data.Monoid and getting shadowed names above
  (<>) = mappend

  f'' :: S.Set Ident -> Declaration a b -> r
  f'' s a = f s a <> f' s a

  f' :: S.Set Ident -> Declaration a b -> r
  f' s (DataBindingGroupDeclaration ds _) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds
  f' s (ValueDeclaration name _ bs val _) =
    let s' = S.insert name s
        s'' = S.union s' (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s') bs <> foldMap (l' s'') val
  f' s (BindingGroupDeclaration ds _) =
    let s' = S.union s (S.fromList (map (\(name, _, _) -> name) ds))
    in foldMap (\(_, _, val) -> g'' s' val) ds
  f' s (TypeClassDeclaration _ _ _ _ ds _) = foldMap (f'' s) ds
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds) _) = foldMap (f'' s) ds
  f' s (PositionedDeclaration _ _ d _) = f'' s d
  f' _ _ = mempty

  g'' :: S.Set Ident -> Expr a b -> r
  g'' s a = g s a <> g' s a

  g' :: S.Set Ident -> Expr a b -> r
  g' s (Literal l _) = lit g'' s l
  g' s (UnaryMinus v1 _) = g'' s v1
  g' s (BinaryNoParens op v1 v2 _) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1 _) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ v1 _) = g'' s v1
  g' s (Accessor _ v1 _) = g'' s v1
  g' s (ObjectUpdate obj vs _) = g'' s obj <> foldMap (g'' s . snd) vs
  g' s (ObjectUpdateNested obj vs _) = g'' s obj <> foldMap (g'' s) vs
  g' s (Abs (Left name) v1 _) =
    let s' = S.insert name s
    in g'' s' v1
  g' s (Abs (Right b) v1 _) =
    let s' = S.union (S.fromList (binderNames b)) s
    in g'' s' v1
  g' s (App v1 v2 _) = g'' s v1 <> g'' s v2
  g' s (IfThenElse v1 v2 v3 _) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts _) = foldMap (g'' s) vs <> foldMap (i'' s) alts
  g' s (TypedValue _ v1 _ _) = g'' s v1
  g' s (Let ds v1 _) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds <> g'' s' v1
  g' s (Do es _) = fold . snd . mapAccumL j'' s $ es
  g' s (PositionedValue _ _ v1 _) = g'' s v1
  g' _ _ = mempty

  h'' :: S.Set Ident -> Binder a b -> r
  h'' s a = h s a <> h' s a

  h' :: S.Set Ident -> Binder a b -> r
  h' s (LiteralBinder l _) = lit h'' s l
  h' s (ConstructorBinder _ bs _) = foldMap (h'' s) bs
  h' s (BinaryNoParensBinder b1 b2 b3 _) = foldMap (h'' s) [b1, b2, b3]
  h' s (ParensInBinder b _) = h'' s b
  h' s (NamedBinder name b1 _) = h'' (S.insert name s) b1
  h' s (PositionedBinder _ _ b1 _) = h'' s b1
  h' s (TypedBinder _ b1 _) = h'' s b1
  h' _ _ = mempty

  lit :: (S.Set Ident -> x -> r) -> S.Set Ident -> Literal x -> r
  lit go s (ArrayLiteral as) = foldMap (go s) as
  lit go s (ObjectLiteral as) = foldMap (go s . snd) as
  lit _ _ _ = mempty

  i'' :: S.Set Ident -> CaseAlternative a b -> r
  i'' s a = i s a <> i' s a

  i' :: S.Set Ident -> CaseAlternative a b -> r
  i' s (CaseAlternative bs gs) =
    let s' = S.union s (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s) bs <> foldMap (l' s') gs

  j'' :: S.Set Ident -> DoNotationElement a b -> (S.Set Ident, r)
  j'' s a = let (s', r) = j' s a in (s', j s a <> r)

  j' :: S.Set Ident -> DoNotationElement a b -> (S.Set Ident, r)
  j' s (DoNotationValue v _) = (s, g'' s v)
  j' s (DoNotationBind b v _) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s v)
  j' s (DoNotationLet ds _) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in (s', foldMap (f'' s') ds)
  j' s (PositionedDoNotationElement _ _ e1 _) = j'' s e1

  k' :: S.Set Ident -> Guard a b -> (S.Set Ident, r)
  k' s (ConditionGuard e) = (s, g'' s e)
  k' s (PatternGuard b e) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s' e)

  l' s (GuardedExpr [] e) = g'' s e
  l' s (GuardedExpr (grd:gs) e) =
    let (s', r) = k' s grd
    in r <> l' s' (GuardedExpr gs e)

  getDeclIdent :: Declaration a b -> Maybe Ident
  getDeclIdent (PositionedDeclaration _ _ d _) = getDeclIdent d
  getDeclIdent (ValueDeclaration ident _ _ _ _) = Just ident
  getDeclIdent (TypeDeclaration ident _ _) = Just ident
  getDeclIdent _ = Nothing

accumTypes
  :: (Monoid r)
  => (Type a -> r)
  -> ( Declaration a b -> r
     , Expr a b -> r
     , Binder a b -> r
     , CaseAlternative a b -> r
     , DoNotationElement a b -> r
     )
accumTypes f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ dctors _) = mconcat (concatMap (map f . snd) dctors)
  forDecls (ExternDeclaration _ ty _) = f ty
  forDecls (TypeClassDeclaration _ _ implies _ _ _) = mconcat (concatMap (map f . constraintArgs) implies)
  forDecls (TypeInstanceDeclaration _ cs _ tys _ _) = mconcat (concatMap (map f . constraintArgs) cs) `mappend` mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ ty _) = f ty
  forDecls (TypeDeclaration _ ty _) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary c _ _ _) = mconcat (map f (constraintArgs c))
  forValues (DeferredDictionary _ tys _) = mconcat (map f tys)
  forValues (TypedValue _ _ ty _) = f ty
  forValues _ = mempty

accumKinds
  :: (Monoid r)
  => (Kind -> r)
  -> ( Declaration a b -> r
     , Expr a b -> r
     , Binder a b -> r
     , CaseAlternative a b -> r
     , DoNotationElement a b -> r
     )
accumKinds f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ args dctors _) =
    foldMap (foldMap f . snd) args `mappend`
    foldMap (foldMap forTypes . snd) dctors
  forDecls (TypeClassDeclaration _ args implies _ _ _) =
    foldMap (foldMap f . snd) args `mappend`
    foldMap (foldMap forTypes . constraintArgs) implies
  forDecls (TypeInstanceDeclaration _ cs _ tys _ _) =
    foldMap (foldMap forTypes . constraintArgs) cs `mappend`
    foldMap forTypes tys
  forDecls (TypeSynonymDeclaration _ args ty _) =
    foldMap (foldMap f . snd) args `mappend`
    forTypes ty
  forDecls (TypeDeclaration _ ty _) = forTypes ty
  forDecls (ExternDeclaration _ ty _) = forTypes ty
  forDecls (ExternDataDeclaration _ kn _) = f kn
  forDecls _ = mempty

  forValues (TypeClassDictionary c _ _ _) = foldMap forTypes (constraintArgs c)
  forValues (DeferredDictionary _ tys _) = foldMap forTypes tys
  forValues (TypedValue _ _ ty _) = forTypes ty
  forValues _ = mempty

  forTypes (KindedType _ k _) = f k
  forTypes _ = mempty

-- |
-- Map a function over type annotations appearing inside a value
--
overTypes :: forall a b. (Type a -> Type a) -> Expr a b -> Expr a b
overTypes f = let (_, f', _) = everywhereOnValues id g id in f'
  where
  g :: Expr a b -> Expr a b
  g (TypedValue checkTy val t b) = TypedValue checkTy val (f t) b
  g (TypeClassDictionary c sco hints b) = TypeClassDictionary (mapConstraintArgs (map f) c) sco hints b
  g other = other
