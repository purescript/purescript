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
  f' (DataBindingGroupDeclaration ann ds) = f (DataBindingGroupDeclaration ann (map f' ds))
  f' (ValueDeclaration ann name nameKind bs val) = f (ValueDeclaration ann name nameKind (map h' bs) (map (mapGuardedExpr handleGuard g') val))
  f' (BindingGroupDeclaration ann ds) = f (BindingGroupDeclaration ann (map (\(name, nameKind, val) -> (name, nameKind, g' val)) ds))
  f' (TypeClassDeclaration ann name args implies deps ds) = f (TypeClassDeclaration ann name args implies deps (map f' ds))
  f' (TypeInstanceDeclaration ann name cs className args ds) = f (TypeInstanceDeclaration ann name cs className args (mapTypeInstanceBody (map f') ds))
  f' (PositionedDeclaration ann pos com d) = f (PositionedDeclaration ann pos com (f' d))
  f' other = f other

  g' :: Expr a b -> Expr a b
  g' (Literal ann l) = g (Literal ann (lit g' l))
  g' (UnaryMinus ann v) = g (UnaryMinus ann (g' v))
  g' (BinaryNoParens ann op v1 v2) = g (BinaryNoParens ann (g' op) (g' v1) (g' v2))
  g' (Parens ann v) = g (Parens ann (g' v))
  g' (TypeClassDictionaryConstructorApp ann name v) = g (TypeClassDictionaryConstructorApp ann name (g' v))
  g' (Accessor ann prop v) = g (Accessor ann prop (g' v))
  g' (ObjectUpdate ann obj vs) = g (ObjectUpdate ann (g' obj) (map (fmap g') vs))
  g' (ObjectUpdateNested ann obj vs) = g (ObjectUpdateNested ann (g' obj) (fmap g' vs))
  g' (Abs ann name v) = g (Abs ann name (g' v))
  g' (App ann v1 v2) = g (App ann (g' v1) (g' v2))
  g' (IfThenElse ann v1 v2 v3) = g (IfThenElse ann (g' v1) (g' v2) (g' v3))
  g' (Case ann vs alts) = g (Case ann (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue ann check v ty) = g (TypedValue ann check (g' v) ty)
  g' (Let ann ds v) = g (Let ann (map f' ds) (g' v))
  g' (Do ann es) = g (Do ann (map handleDoNotationElement es))
  g' (PositionedValue ann pos com v) = g (PositionedValue ann pos com (g' v))
  g' other = g other

  h' :: Binder a b -> Binder a b
  h' (ConstructorBinder ann ctor bs) = h (ConstructorBinder ann ctor (map h' bs))
  h' (BinaryNoParensBinder ann b1 b2 b3) = h (BinaryNoParensBinder ann (h' b1) (h' b2) (h' b3))
  h' (ParensInBinder ann b) = h (ParensInBinder ann (h' b))
  h' (LiteralBinder ann l) = h (LiteralBinder ann (lit h' l))
  h' (NamedBinder ann name b) = h (NamedBinder ann name (h' b))
  h' (PositionedBinder ann pos com b) = h (PositionedBinder ann pos com (h' b))
  h' (TypedBinder ann t b) = h (TypedBinder ann t (h' b))
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
  handleDoNotationElement (DoNotationValue ann v) = DoNotationValue ann (g' v)
  handleDoNotationElement (DoNotationBind ann b v) = DoNotationBind ann (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ann ds) = DoNotationLet ann (map f' ds)
  handleDoNotationElement (PositionedDoNotationElement ann pos com e) = PositionedDoNotationElement ann pos com (handleDoNotationElement e)

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
  f' (DataBindingGroupDeclaration ann ds) = DataBindingGroupDeclaration ann <$> traverse (f' <=< f) ds
  f' (ValueDeclaration ann name nameKind bs val) = ValueDeclaration ann name nameKind <$> traverse (h' <=< h) bs <*> traverse (guardedExprM handleGuard (g' <=< g)) val
  f' (BindingGroupDeclaration ann ds) = BindingGroupDeclaration ann <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration ann name args implies deps ds) = TypeClassDeclaration ann name args implies deps <$> traverse (f' <=< f) ds
  f' (TypeInstanceDeclaration ann name cs className args ds) = TypeInstanceDeclaration ann name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds
  f' (PositionedDeclaration ann pos com d) = PositionedDeclaration ann pos com <$> (f d >>= f')
  f' other = f other

  g' :: Expr a b -> m (Expr a b)
  g' (Literal ann l) = Literal ann <$> lit (g >=> g') l
  g' (UnaryMinus ann v) = UnaryMinus ann <$> (g v >>= g')
  g' (BinaryNoParens ann op v1 v2) = BinaryNoParens ann <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens ann v) = Parens ann <$> (g v >>= g')
  g' (TypeClassDictionaryConstructorApp ann name v) = TypeClassDictionaryConstructorApp ann name <$> (g v >>= g')
  g' (Accessor ann prop v) = Accessor ann prop <$> (g v >>= g')
  g' (ObjectUpdate ann obj vs) = ObjectUpdate ann <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs
  g' (ObjectUpdateNested ann obj vs) = ObjectUpdateNested ann <$> (g obj >>= g') <*> traverse (g' <=< g) vs
  g' (Abs ann name v) = Abs ann name <$> (g v >>= g')
  g' (App ann v1 v2) = App ann <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (IfThenElse ann v1 v2 v3) = IfThenElse ann <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case ann vs alts) = Case ann <$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts
  g' (TypedValue ann check v ty) = TypedValue ann check <$> (g v >>= g') <*> pure ty
  g' (Let ann ds v) = Let ann <$> traverse (f' <=< f) ds <*> (g v >>= g')
  g' (Do ann es) = Do ann <$> traverse handleDoNotationElement es
  g' (PositionedValue ann pos com v) = PositionedValue ann pos com <$> (g v >>= g')
  g' other = g other

  h' :: Binder a b -> m (Binder a b)
  h' (LiteralBinder ann l) = LiteralBinder ann <$> lit (h >=> h') l
  h' (ConstructorBinder ann ctor bs) = ConstructorBinder ann ctor <$> traverse (h' <=< h) bs
  h' (BinaryNoParensBinder ann b1 b2 b3) = BinaryNoParensBinder ann <$> (h b1 >>= h') <*> (h b2 >>= h') <*> (h b3 >>= h')
  h' (ParensInBinder ann b) = ParensInBinder ann <$> (h b >>= h')
  h' (NamedBinder ann name b) = NamedBinder ann name <$> (h b >>= h')
  h' (PositionedBinder ann pos com b) = PositionedBinder ann pos com <$> (h b >>= h')
  h' (TypedBinder ann t b) = TypedBinder ann t <$> (h b >>= h')
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
  handleDoNotationElement (DoNotationValue ann v) = DoNotationValue ann <$> (g' <=< g) v
  handleDoNotationElement (DoNotationBind ann b v) = DoNotationBind ann <$> (h' <=< h) b <*> (g' <=< g) v
  handleDoNotationElement (DoNotationLet ann ds) = DoNotationLet ann <$> traverse (f' <=< f) ds
  handleDoNotationElement (PositionedDoNotationElement ann pos com e) = PositionedDoNotationElement ann pos com <$> handleDoNotationElement e

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
  f' (DataBindingGroupDeclaration ann ds) = (DataBindingGroupDeclaration ann <$> traverse f' ds) >>= f
  f' (ValueDeclaration ann name nameKind bs val) = (ValueDeclaration ann name nameKind <$> traverse h' bs <*> traverse (guardedExprM handleGuard g') val) >>= f
  f' (BindingGroupDeclaration ann ds) = (BindingGroupDeclaration ann <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds) >>= f
  f' (TypeClassDeclaration ann name args implies deps ds) = (TypeClassDeclaration ann name args implies deps <$> traverse f' ds) >>= f
  f' (TypeInstanceDeclaration ann name cs className args ds) = (TypeInstanceDeclaration ann name cs className args <$> traverseTypeInstanceBody (traverse f') ds) >>= f
  f' (PositionedDeclaration ann pos com d) = (PositionedDeclaration ann pos com <$> f' d) >>= f
  f' other = f other

  g' :: Expr a b -> m (Expr a b)
  g' (Literal ann l) = (Literal ann <$> lit g' l) >>= g
  g' (UnaryMinus ann v) = (UnaryMinus ann <$> g' v) >>= g
  g' (BinaryNoParens ann op v1 v2) = (BinaryNoParens ann <$> g' op <*> g' v1 <*> g' v2) >>= g
  g' (Parens ann v) = (Parens ann <$> g' v) >>= g
  g' (TypeClassDictionaryConstructorApp ann name v) = (TypeClassDictionaryConstructorApp ann name <$> g' v) >>= g
  g' (Accessor ann prop v) = (Accessor ann prop <$> g' v) >>= g
  g' (ObjectUpdate ann obj vs) = (ObjectUpdate ann <$> g' obj <*> traverse (sndM g') vs) >>= g
  g' (ObjectUpdateNested ann obj vs) = (ObjectUpdateNested ann <$> g' obj <*> traverse g' vs) >>= g
  g' (Abs ann name v) = (Abs ann name <$> g' v) >>= g
  g' (App ann v1 v2) = (App ann <$> g' v1 <*> g' v2) >>= g
  g' (IfThenElse ann v1 v2 v3) = (IfThenElse ann <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case ann vs alts) = (Case ann <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
  g' (TypedValue ann check v ty) = (TypedValue ann check <$> g' v <*> pure ty) >>= g
  g' (Let ann ds v) = (Let ann <$> traverse f' ds <*> g' v) >>= g
  g' (Do ann es) = (Do ann <$> traverse handleDoNotationElement es) >>= g
  g' (PositionedValue ann pos com v) = (PositionedValue ann pos com <$> g' v) >>= g
  g' other = g other

  h' :: Binder a b -> m (Binder a b)
  h' (LiteralBinder ann l) = (LiteralBinder ann <$> lit h' l) >>= h
  h' (ConstructorBinder ann ctor bs) = (ConstructorBinder ann ctor <$> traverse h' bs) >>= h
  h' (BinaryNoParensBinder ann b1 b2 b3) = (BinaryNoParensBinder ann <$> h' b1 <*> h' b2 <*> h' b3) >>= h
  h' (ParensInBinder ann b) = (ParensInBinder ann <$> h' b) >>= h
  h' (NamedBinder ann name b) = (NamedBinder ann name <$> h' b) >>= h
  h' (PositionedBinder ann pos com b) = (PositionedBinder ann pos com <$> h' b) >>= h
  h' (TypedBinder ann t b) = (TypedBinder ann t <$> h' b) >>= h
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
  handleDoNotationElement (DoNotationValue ann v) = DoNotationValue ann <$> g' v
  handleDoNotationElement (DoNotationBind ann b v) = DoNotationBind ann <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ann ds) = DoNotationLet ann <$> traverse f' ds
  handleDoNotationElement (PositionedDoNotationElement ann pos com e) = PositionedDoNotationElement ann pos com <$> handleDoNotationElement e

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
  f' d@(DataBindingGroupDeclaration _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ _ bs val) = foldl (<>) (f d) (map h' bs ++ concatMap (\(GuardedExpr grd v) -> map k' grd ++ [g' v]) val)
  f' d@(BindingGroupDeclaration _ ds) = foldl (<>) (f d) (map (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ _ (ExplicitInstance ds)) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ _ _ d1) = f d <> f' d1
  f' d = f d

  g' :: Expr a b -> r
  g' v@(Literal _ l) = lit (g v) g' l
  g' v@(UnaryMinus _ v1) = g v <> g' v1
  g' v@(BinaryNoParens _ op v1 v2) = g v <> g' op <> g' v1 <> g' v2
  g' v@(Parens _ v1) = g v <> g' v1
  g' v@(TypeClassDictionaryConstructorApp _ _ v1) = g v <> g' v1
  g' v@(Accessor _ _ v1) = g v <> g' v1
  g' v@(ObjectUpdate _ obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(ObjectUpdateNested _ obj vs) = foldl (<>) (g v <> g' obj) (fmap g' vs)
  g' v@(Abs _ _ v1) = g v <> g' v1
  g' v@(App _ v1 v2) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse _ v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case _ vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ _ v1 _) = g v <> g' v1
  g' v@(Let _ ds v1) = foldl (<>) (g v) (map f' ds) <> g' v1
  g' v@(Do _ es) = foldl (<>) (g v) (map j' es)
  g' v@(PositionedValue _ _ _ v1) = g v <> g' v1
  g' v = g v

  h' :: Binder a b -> r
  h' b@(LiteralBinder _ l) = lit (h b) h' l
  h' b@(ConstructorBinder _ _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(BinaryNoParensBinder _ b1 b2 b3) = h b <> h' b1 <> h' b2 <> h' b3
  h' b@(ParensInBinder _ b1) = h b <> h' b1
  h' b@(NamedBinder _ _ b1) = h b <> h' b1
  h' b@(PositionedBinder _ _ _ b1) = h b <> h' b1
  h' b@(TypedBinder _ _ b1) = h b <> h' b1
  h' b = h b

  lit :: r -> (x -> r) -> Literal x -> r
  lit r go (ArrayLiteral as) = foldl (<>) r (map go as)
  lit r go (ObjectLiteral as) = foldl (<>) r (map (go . snd) as)
  lit r _ _ = r

  i' :: CaseAlternative a b -> r
  i' ca@(CaseAlternative bs gs) =
    foldl (<>) (i ca) (map h' bs ++ concatMap (\(GuardedExpr grd val) -> map k' grd ++ [g' val]) gs)

  j' :: DoNotationElement a b -> r
  j' e@(DoNotationValue _ v) = j e <> g' v
  j' e@(DoNotationBind _ b v) = j e <> h' b <> g' v
  j' e@(DoNotationLet _ ds) = foldl (<>) (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ _ _ e1) = j e <> j' e1

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
  f' s (DataBindingGroupDeclaration _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ _ bs val) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(GuardedExpr grd v) -> map (k' s) grd ++ [g'' s v]) val)
  f' s (BindingGroupDeclaration _ ds) = foldl (<>) r0 (map (\(_, _, val) -> g'' s val) ds)
  f' s (TypeClassDeclaration _ _ _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ _ (ExplicitInstance ds)) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ _ _ d1) = f'' s d1
  f' _ _ = r0

  g'' :: s -> Expr a b -> r
  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' :: s -> Expr a b -> r
  g' s (Literal _ l) = lit g'' s l
  g' s (UnaryMinus _ v1) = g'' s v1
  g' s (BinaryNoParens _ op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens _ v1) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ _ v1) = g'' s v1
  g' s (Accessor _ _ v1) = g'' s v1
  g' s (ObjectUpdate _ obj vs) = foldl (<>) (g'' s obj) (map (g'' s . snd) vs)
  g' s (ObjectUpdateNested _ obj vs) = foldl (<>) (g'' s obj) (fmap (g'' s) vs)
  g' s (Abs _ _ v1) = g'' s v1
  g' s (App _ v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse _ v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case _ vs alts) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ _ v1 _) = g'' s v1
  g' s (Let _ ds v1) = foldl (<>) r0 (map (f'' s) ds) <> g'' s v1
  g' s (Do _ es) = foldl (<>) r0 (map (j'' s) es)
  g' s (PositionedValue _ _ _ v1) = g'' s v1
  g' _ _ = r0

  h'' :: s -> Binder a b -> r
  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' :: s -> Binder a b -> r
  h' s (LiteralBinder _ l) = lit h'' s l
  h' s (ConstructorBinder _ _ bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (BinaryNoParensBinder _ b1 b2 b3) = h'' s b1 <> h'' s b2 <> h'' s b3
  h' s (ParensInBinder _ b) = h'' s b
  h' s (NamedBinder _ _ b1) = h'' s b1
  h' s (PositionedBinder _ _ _ b1) = h'' s b1
  h' s (TypedBinder _ _ b1) = h'' s b1
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
  j' s (DoNotationValue _ v) = g'' s v
  j' s (DoNotationBind _ b v) = h'' s b <> g'' s v
  j' s (DoNotationLet _ ds) = foldl (<>) r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ _ _ e1) = j'' s e1

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

  f' s (DataBindingGroupDeclaration ann ds) = DataBindingGroupDeclaration ann <$> traverse (f'' s) ds
  f' s (ValueDeclaration ann name nameKind bs val) = ValueDeclaration ann name nameKind <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val
  f' s (BindingGroupDeclaration ann ds) = BindingGroupDeclaration ann <$> traverse (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration ann name args implies deps ds) = TypeClassDeclaration ann name args implies deps <$> traverse (f'' s) ds
  f' s (TypeInstanceDeclaration ann name cs className args ds) = TypeInstanceDeclaration ann name cs className args <$> traverseTypeInstanceBody (traverse (f'' s)) ds
  f' s (PositionedDeclaration ann pos com d1) = PositionedDeclaration ann pos com <$> f'' s d1
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (Literal ann l) = Literal ann <$> lit g'' s l
  g' s (UnaryMinus ann v) = UnaryMinus ann <$> g'' s v
  g' s (BinaryNoParens ann op v1 v2) = BinaryNoParens ann <$> g'' s op <*> g'' s v1 <*> g'' s v2
  g' s (Parens ann v) = Parens ann <$> g'' s v
  g' s (TypeClassDictionaryConstructorApp ann name v) = TypeClassDictionaryConstructorApp ann name <$> g'' s v
  g' s (Accessor ann prop v) = Accessor ann prop <$> g'' s v
  g' s (ObjectUpdate ann obj vs) = ObjectUpdate ann <$> g'' s obj <*> traverse (sndM (g'' s)) vs
  g' s (ObjectUpdateNested ann obj vs) = ObjectUpdateNested ann <$> g'' s obj <*> traverse (g'' s) vs
  g' s (Abs ann name v) = Abs ann name <$> g'' s v
  g' s (App ann v1 v2) = App ann <$> g'' s v1 <*> g'' s v2
  g' s (IfThenElse ann v1 v2 v3) = IfThenElse ann <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case ann vs alts) = Case ann <$> traverse (g'' s) vs <*> traverse (i'' s) alts
  g' s (TypedValue ann check v ty) = TypedValue ann check <$> g'' s v <*> pure ty
  g' s (Let ann ds v) = Let ann <$> traverse (f'' s) ds <*> g'' s v
  g' s (Do ann es) = Do ann <$> traverse (j'' s) es
  g' s (PositionedValue ann pos com v) = PositionedValue ann pos com <$> g'' s v
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (LiteralBinder ann l) = LiteralBinder ann <$> lit h'' s l
  h' s (ConstructorBinder ann ctor bs) = ConstructorBinder ann ctor <$> traverse (h'' s) bs
  h' s (BinaryNoParensBinder ann b1 b2 b3) = BinaryNoParensBinder ann <$> h'' s b1 <*> h'' s b2 <*> h'' s b3
  h' s (ParensInBinder ann b) = ParensInBinder ann <$> h'' s b
  h' s (NamedBinder ann name b) = NamedBinder ann name <$> h'' s b
  h' s (PositionedBinder ann pos com b) = PositionedBinder ann pos com <$> h'' s b
  h' s (TypedBinder ann t b) = TypedBinder ann t <$> h'' s b
  h' _ other = return other

  lit :: (s -> x -> m x) -> s -> Literal x -> m (Literal x)
  lit go s (ArrayLiteral as) = ArrayLiteral <$> traverse (go s) as
  lit go s (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM (go s)) as
  lit _ _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative bs val) = CaseAlternative <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue ann v) = DoNotationValue ann <$> g'' s v
  j' s (DoNotationBind ann b v) = DoNotationBind ann <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ann ds) = DoNotationLet ann <$> traverse (f'' s) ds
  j' s (PositionedDoNotationElement ann pos com e1) = PositionedDoNotationElement ann pos com <$> j'' s e1

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
  f' s (DataBindingGroupDeclaration _ ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds
  f' s (ValueDeclaration _ name _ bs val) =
    let s' = S.insert name s
        s'' = S.union s' (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s') bs <> foldMap (l' s'') val
  f' s (BindingGroupDeclaration _ ds) =
    let s' = S.union s (S.fromList (map (\(name, _, _) -> name) ds))
    in foldMap (\(_, _, val) -> g'' s' val) ds
  f' s (TypeClassDeclaration _ _ _ _ _ ds) = foldMap (f'' s) ds
  f' s (TypeInstanceDeclaration _ _ _ _ _ (ExplicitInstance ds)) = foldMap (f'' s) ds
  f' s (PositionedDeclaration _ _ _ d) = f'' s d
  f' _ _ = mempty

  g'' :: S.Set Ident -> Expr a b -> r
  g'' s a = g s a <> g' s a

  g' :: S.Set Ident -> Expr a b -> r
  g' s (Literal _ l) = lit g'' s l
  g' s (UnaryMinus _ v1) = g'' s v1
  g' s (BinaryNoParens _ op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens _ v1) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ _ v1) = g'' s v1
  g' s (Accessor _ _ v1) = g'' s v1
  g' s (ObjectUpdate _ obj vs) = g'' s obj <> foldMap (g'' s . snd) vs
  g' s (ObjectUpdateNested _ obj vs) = g'' s obj <> foldMap (g'' s) vs
  g' s (Abs _ (Left name) v1) =
    let s' = S.insert name s
    in g'' s' v1
  g' s (Abs _ (Right b) v1) =
    let s' = S.union (S.fromList (binderNames b)) s
    in g'' s' v1
  g' s (App _ v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse _ v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case _ vs alts) = foldMap (g'' s) vs <> foldMap (i'' s) alts
  g' s (TypedValue _ _ v1 _) = g'' s v1
  g' s (Let _ ds v1) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds <> g'' s' v1
  g' s (Do _ es) = fold . snd . mapAccumL j'' s $ es
  g' s (PositionedValue _ _ _ v1) = g'' s v1
  g' _ _ = mempty

  h'' :: S.Set Ident -> Binder a b -> r
  h'' s a = h s a <> h' s a

  h' :: S.Set Ident -> Binder a b -> r
  h' s (LiteralBinder _ l) = lit h'' s l
  h' s (ConstructorBinder _ _ bs) = foldMap (h'' s) bs
  h' s (BinaryNoParensBinder _ b1 b2 b3) = foldMap (h'' s) [b1, b2, b3]
  h' s (ParensInBinder _ b) = h'' s b
  h' s (NamedBinder _ name b1) = h'' (S.insert name s) b1
  h' s (PositionedBinder _ _ _ b1) = h'' s b1
  h' s (TypedBinder _ _ b1) = h'' s b1
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
  j' s (DoNotationValue _ v) = (s, g'' s v)
  j' s (DoNotationBind _ b v) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s v)
  j' s (DoNotationLet _ ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in (s', foldMap (f'' s') ds)
  j' s (PositionedDoNotationElement _ _ _ e1) = j'' s e1

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
  getDeclIdent (PositionedDeclaration _ _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration _ ident _ _ _) = Just ident
  getDeclIdent (TypeDeclaration _ ident _) = Just ident
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
  forDecls (DataDeclaration _ _ _ _ dctors) = mconcat (concatMap (map f . snd) dctors)
  forDecls (ExternDeclaration _ _ ty) = f ty
  forDecls (TypeClassDeclaration _ _ _ implies _ _) = mconcat (concatMap (map f . constraintArgs) implies)
  forDecls (TypeInstanceDeclaration _ _ cs _ tys _) = mconcat (concatMap (map f . constraintArgs) cs) `mappend` mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ _ ty) = f ty
  forDecls (TypeDeclaration _ _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary _ c _ _) = mconcat (map f (constraintArgs c))
  forValues (DeferredDictionary _ _ tys) = mconcat (map f tys)
  forValues (TypedValue _ _ _ ty) = f ty
  forValues _ = mempty

accumKinds
  :: forall a b r. (Monoid r)
  => (Kind -> r)
  -> ( Declaration a b -> r
     , Expr a b -> r
     , Binder a b -> r
     , CaseAlternative a b -> r
     , DoNotationElement a b -> r
     )
accumKinds f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ args dctors) =
    foldMap (foldMap f . snd) args `mappend`
    foldMap (foldMap forTypes . snd) dctors
  forDecls (TypeClassDeclaration _ _ args implies _ _) =
    foldMap (foldMap f . snd) args `mappend`
    foldMap (foldMap forTypes . constraintArgs) implies
  forDecls (TypeInstanceDeclaration _ _ cs _ tys _) =
    foldMap (foldMap forTypes . constraintArgs) cs `mappend`
    foldMap forTypes tys
  forDecls (TypeSynonymDeclaration _ _ args ty) =
    foldMap (foldMap f . snd) args `mappend`
    forTypes ty
  forDecls (TypeDeclaration _ _ ty) = forTypes ty
  forDecls (ExternDeclaration _ _ ty) = forTypes ty
  forDecls (ExternDataDeclaration _ _ kn) = f kn
  forDecls _ = mempty

  forValues (TypeClassDictionary _ c _ _) = foldMap forTypes (constraintArgs c)
  forValues (DeferredDictionary _ _ tys) = foldMap forTypes tys
  forValues (TypedValue _ _ _ ty) = forTypes ty
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
  g (TypedValue ann checkTy val t) = TypedValue ann checkTy val (f t)
  g (TypeClassDictionary ann c sco hints) = TypeClassDictionary ann (mapConstraintArgs (map f) c) sco hints
  g other = other
