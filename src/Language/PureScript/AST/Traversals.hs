-- |
-- AST traversal helpers
--
module Language.PureScript.AST.Traversals where

import Prelude.Compat

import Control.Monad

import Data.Foldable (fold)
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as S

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Literals
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.Types

guardedExprM :: Applicative m
             => (Guard -> m Guard)
             -> (Expr -> m Expr)
             -> GuardedExpr
             -> m GuardedExpr
guardedExprM f g (GuardedExpr ss guards rhs) =
  GuardedExpr ss <$> traverse f guards <*> g rhs

mapGuardedExpr :: (Guard -> Guard)
               -> (Expr -> Expr)
               -> GuardedExpr
               -> GuardedExpr
mapGuardedExpr f g (GuardedExpr ss guards rhs) =
  GuardedExpr ss (fmap f guards) (g rhs)

litM :: Monad m => (a -> m a) -> Literal a -> m (Literal a)
litM go (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM go) as
litM go (ArrayLiteral as) = ArrayLiteral <$> traverse go as
litM _ other = pure other

everywhereOnValues
  :: (Declaration -> Declaration)
  -> (Expr -> Expr)
  -> (Binder -> Binder)
  -> ( Declaration -> Declaration
     , Expr -> Expr
     , Binder -> Binder
     )
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (fmap f' ds))
  f' (ValueDeclaration sa name nameKind bs val) = f (ValueDeclaration sa name nameKind (fmap h' bs) (fmap (mapGuardedExpr handleGuard g') val))
  f' (BoundValueDeclaration sa b expr) = f (BoundValueDeclaration sa (h' b) (g' expr))
  f' (BindingGroupDeclaration ds) = f (BindingGroupDeclaration (fmap (\(name, nameKind, val) -> (name, nameKind, g' val)) ds))
  f' (TypeClassDeclaration sa name args implies deps ds) = f (TypeClassDeclaration sa name args implies deps (fmap f' ds))
  f' (TypeInstanceDeclaration sa name cs className args ds) = f (TypeInstanceDeclaration sa name cs className args (mapTypeInstanceBody (fmap f') ds))
  f' other = f other

  g' :: Expr -> Expr
  g' (Literal ss l) = g (Literal ss (lit g' l))
  g' (UnaryMinus ss v) = g (UnaryMinus ss (g' v))
  g' (BinaryNoParens ss op v1 v2) = g (BinaryNoParens ss (g' op) (g' v1) (g' v2))
  g' (Parens ss v) = g (Parens ss (g' v))
  g' (TypeClassDictionaryConstructorApp ss name v) = g (TypeClassDictionaryConstructorApp ss name (g' v))
  g' (Accessor ss prop v) = g (Accessor ss prop (g' v))
  g' (ObjectUpdate ss obj vs) = g (ObjectUpdate ss (g' obj) (fmap (fmap g') vs))
  g' (ObjectUpdateNested ss obj vs) = g (ObjectUpdateNested ss (g' obj) (fmap g' vs))
  g' (Abs ss binder v) = g (Abs ss (h' binder) (g' v))
  g' (App ss v1 v2) = g (App ss (g' v1) (g' v2))
  g' (IfThenElse ss v1 v2 v3) = g (IfThenElse ss (g' v1) (g' v2) (g' v3))
  g' (Case ss vs alts) = g (Case ss (fmap g' vs) (fmap handleCaseAlternative alts))
  g' (TypedValue ss check v ty) = g (TypedValue ss check (g' v) ty)
  g' (Let ss ds v) = g (Let ss (fmap f' ds) (g' v))
  g' (Do ss es) = g (Do ss (fmap handleDoNotationElement es))
  g' other = g other

  h' :: Binder -> Binder
  h' (ConstructorBinder ss ctor bs) = h (ConstructorBinder ss ctor (fmap h' bs))
  h' (BinaryNoParensBinder ss b1 b2 b3) = h (BinaryNoParensBinder ss (h' b1) (h' b2) (h' b3))
  h' (ParensInBinder ss b) = h (ParensInBinder ss (h' b))
  h' (LiteralBinder ss l) = h (LiteralBinder ss (lit h' l))
  h' (NamedBinder ss name b) = h (NamedBinder ss name (h' b))
  h' (TypedBinder ss t b) = h (TypedBinder ss t (h' b))
  h' other = h other

  lit :: (a -> a) -> Literal a -> Literal a
  lit go (ArrayLiteral as) = ArrayLiteral (fmap go as)
  lit go (ObjectLiteral as) = ObjectLiteral (fmap (fmap go) as)
  lit _ other = other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = fmap h' (caseAlternativeBinders ca)
       , caseAlternativeResult = fmap (mapGuardedExpr handleGuard g') (caseAlternativeResult ca)
       }

  handleDoNotationElement :: DoNotationElement -> DoNotationElement
  handleDoNotationElement (DoNotationValue ss v) = DoNotationValue ss (g' v)
  handleDoNotationElement (DoNotationBind ss b v) = DoNotationBind ss (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ss ds) = DoNotationLet ss (fmap f' ds)

  handleGuard :: Guard -> Guard
  handleGuard (ConditionGuard ss e) = ConditionGuard ss (g' e)
  handleGuard (PatternGuard ss b e) = PatternGuard ss (h' b) (g' e)

everywhereOnValuesTopDownM
  :: forall m
   . (Monad m)
  => (Declaration -> m Declaration)
  -> (Expr -> m Expr)
  -> (Binder -> m Binder)
  -> ( Declaration -> m Declaration
     , Expr -> m Expr
     , Binder -> m Binder
     )
everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where

  f' :: Declaration -> m Declaration
  f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f' <=< f) ds
  f' (ValueDeclaration sa name nameKind bs val) = ValueDeclaration sa name nameKind <$> traverse (h' <=< h) bs <*> traverse (guardedExprM handleGuard (g' <=< g)) val
  f' (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration sa name args implies deps ds) = TypeClassDeclaration sa name args implies deps <$> traverse (f' <=< f) ds
  f' (TypeInstanceDeclaration sa name cs className args ds) = TypeInstanceDeclaration sa name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds
  f' (BoundValueDeclaration sa b expr) = BoundValueDeclaration sa <$> h' b <*> g' expr
  f' other = f other

  g' :: Expr -> m Expr
  g' (Literal ss l) = Literal ss <$> litM (g >=> g') l
  g' (UnaryMinus ss v) = UnaryMinus ss <$> (g v >>= g')
  g' (BinaryNoParens ss op v1 v2) = BinaryNoParens ss <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens ss v) = Parens ss <$> (g v >>= g')
  g' (TypeClassDictionaryConstructorApp ss name v) = TypeClassDictionaryConstructorApp ss name <$> (g v >>= g')
  g' (Accessor ss prop v) = Accessor ss prop <$> (g v >>= g')
  g' (ObjectUpdate ss obj vs) = ObjectUpdate ss <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs
  g' (ObjectUpdateNested ss obj vs) = ObjectUpdateNested ss <$> (g obj >>= g') <*> traverse (g' <=< g) vs
  g' (Abs ss binder v) = Abs ss <$> (h binder >>= h') <*> (g v >>= g')
  g' (App ss v1 v2) = App ss <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (IfThenElse ss v1 v2 v3) = IfThenElse ss <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case ss vs alts) = Case  ss<$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts
  g' (TypedValue ss check v ty) = TypedValue ss check <$> (g v >>= g') <*> pure ty
  g' (Let ss ds v) = Let ss <$> traverse (f' <=< f) ds <*> (g v >>= g')
  g' (Do ss es) = Do ss <$> traverse handleDoNotationElement es
  g' other = g other

  h' :: Binder -> m Binder
  h' (LiteralBinder ss l) = LiteralBinder ss <$> litM (h >=> h') l
  h' (ConstructorBinder ss ctor bs) = ConstructorBinder ss ctor <$> traverse (h' <=< h) bs
  h' (BinaryNoParensBinder ss b1 b2 b3) = BinaryNoParensBinder ss <$> (h b1 >>= h') <*> (h b2 >>= h') <*> (h b3 >>= h')
  h' (ParensInBinder ss b) = ParensInBinder ss <$> (h b >>= h')
  h' (NamedBinder ss name b) = NamedBinder ss name <$> (h b >>= h')
  h' (TypedBinder ss t b) = TypedBinder ss t <$> (h b >>= h')
  h' other = h other

  handleCaseAlternative :: CaseAlternative -> m CaseAlternative
  handleCaseAlternative (CaseAlternative ss bs val) =
    CaseAlternative ss
      <$> traverse (h' <=< h) bs
      <*> traverse (guardedExprM handleGuard (g' <=< g)) val

  handleDoNotationElement :: DoNotationElement -> m DoNotationElement
  handleDoNotationElement (DoNotationValue ss v) = DoNotationValue ss <$> (g' <=< g) v
  handleDoNotationElement (DoNotationBind ss b v) = DoNotationBind ss <$> (h' <=< h) b <*> (g' <=< g) v
  handleDoNotationElement (DoNotationLet ss ds) = DoNotationLet ss <$> traverse (f' <=< f) ds

  handleGuard :: Guard -> m Guard
  handleGuard (ConditionGuard ss e) = ConditionGuard ss <$> (g' <=< g) e
  handleGuard (PatternGuard ss b e) = PatternGuard ss <$> (h' <=< h) b <*> (g' <=< g) e

everywhereOnValuesM
  :: forall m
   . (Monad m)
  => (Declaration -> m Declaration)
  -> (Expr -> m Expr)
  -> (Binder -> m Binder)
  -> ( Declaration -> m Declaration
     , Expr -> m Expr
     , Binder -> m Binder
     )
everywhereOnValuesM f g h = (f', g', h')
  where

  f' :: Declaration -> m Declaration
  f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> traverse f' ds) >>= f
  f' (ValueDeclaration sa name nameKind bs val) = (ValueDeclaration sa name nameKind <$> traverse h' bs <*> traverse (guardedExprM handleGuard g') val) >>= f
  f' (BindingGroupDeclaration ds) = (BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds) >>= f
  f' (BoundValueDeclaration sa b expr) = (BoundValueDeclaration sa <$> h' b <*> g' expr) >>= f
  f' (TypeClassDeclaration sa name args implies deps ds) = (TypeClassDeclaration sa name args implies deps <$> traverse f' ds) >>= f
  f' (TypeInstanceDeclaration sa name cs className args ds) = (TypeInstanceDeclaration sa name cs className args <$> traverseTypeInstanceBody (traverse f') ds) >>= f
  f' other = f other

  g' :: Expr -> m Expr
  g' (Literal ss l) = (Literal ss <$> litM g' l) >>= g
  g' (UnaryMinus ss v) = (UnaryMinus ss <$> g' v) >>= g
  g' (BinaryNoParens ss op v1 v2) = (BinaryNoParens ss <$> g' op <*> g' v1 <*> g' v2) >>= g
  g' (Parens ss v) = (Parens ss <$> g' v) >>= g
  g' (TypeClassDictionaryConstructorApp ss name v) = (TypeClassDictionaryConstructorApp ss name <$> g' v) >>= g
  g' (Accessor ss prop v) = (Accessor ss prop <$> g' v) >>= g
  g' (ObjectUpdate ss obj vs) = (ObjectUpdate ss <$> g' obj <*> traverse (sndM g') vs) >>= g
  g' (ObjectUpdateNested ss obj vs) = (ObjectUpdateNested ss <$> g' obj <*> traverse g' vs) >>= g
  g' (Abs ss binder v) = (Abs ss <$> h' binder <*> g' v) >>= g
  g' (App ss v1 v2) = (App ss <$> g' v1 <*> g' v2) >>= g
  g' (IfThenElse ss v1 v2 v3) = (IfThenElse ss <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case ss vs alts) = (Case ss <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
  g' (TypedValue ss check v ty) = (TypedValue ss check <$> g' v <*> pure ty) >>= g
  g' (Let ss ds v) = (Let ss <$> traverse f' ds <*> g' v) >>= g
  g' (Do ss es) = (Do ss <$> traverse handleDoNotationElement es) >>= g
  g' other = g other

  h' :: Binder -> m Binder
  h' (LiteralBinder ss l) = (LiteralBinder ss <$> litM h' l) >>= h
  h' (ConstructorBinder ss ctor bs) = (ConstructorBinder ss ctor <$> traverse h' bs) >>= h
  h' (BinaryNoParensBinder ss b1 b2 b3) = (BinaryNoParensBinder ss <$> h' b1 <*> h' b2 <*> h' b3) >>= h
  h' (ParensInBinder ss b) = (ParensInBinder ss <$> h' b) >>= h
  h' (NamedBinder ss name b) = (NamedBinder ss name <$> h' b) >>= h
  h' (TypedBinder ss t b) = (TypedBinder ss t <$> h' b) >>= h
  h' other = h other

  handleCaseAlternative :: CaseAlternative -> m CaseAlternative
  handleCaseAlternative (CaseAlternative ss bs val) =
    CaseAlternative ss
      <$> traverse h' bs
      <*> traverse (guardedExprM handleGuard g') val

  handleDoNotationElement :: DoNotationElement -> m DoNotationElement
  handleDoNotationElement (DoNotationValue ss v) = DoNotationValue ss <$> g' v
  handleDoNotationElement (DoNotationBind ss b v) = DoNotationBind ss <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ss ds) = DoNotationLet ss <$> traverse f' ds

  handleGuard :: Guard -> m Guard
  handleGuard (ConditionGuard ss e) = ConditionGuard ss <$> g' e
  handleGuard (PatternGuard ss b e) = PatternGuard ss <$> h' b <*> g' e

everythingOnValues
  :: forall r
   . (r -> r -> r)
  -> (Declaration -> r)
  -> (Expr -> r)
  -> (Binder -> r)
  -> (CaseAlternative -> r)
  -> (DoNotationElement -> r)
  -> ( Declaration -> r
     , Expr -> r
     , Binder -> r
     , CaseAlternative -> r
     , DoNotationElement -> r
     )
everythingOnValues (<>) f g h i j = (f', g', h', i', j')
  where

  f' :: Declaration -> r
  f' d@(DataBindingGroupDeclaration ds) = foldl (<>) (f d) (fmap f' ds)
  f' d@(ValueDeclaration _ _ _ bs val) = foldl (<>) (f d) (fmap h' bs ++ concatMap (\(GuardedExpr _ grd v) -> fmap k' grd ++ [g' v]) val)
  f' d@(BindingGroupDeclaration ds) = foldl (<>) (f d) (fmap (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ _ _ ds) = foldl (<>) (f d) (fmap f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ _ (ExplicitInstance ds)) = foldl (<>) (f d) (fmap f' ds)
  f' d@(BoundValueDeclaration _ b expr) = f d <> h' b <> g' expr
  f' d = f d

  g' :: Expr -> r
  g' v@(Literal _ l) = lit (g v) g' l
  g' v@(UnaryMinus _ v1) = g v <> g' v1
  g' v@(BinaryNoParens _ op v1 v2) = g v <> g' op <> g' v1 <> g' v2
  g' v@(Parens _ v1) = g v <> g' v1
  g' v@(TypeClassDictionaryConstructorApp _ _ v1) = g v <> g' v1
  g' v@(Accessor _ _ v1) = g v <> g' v1
  g' v@(ObjectUpdate _ obj vs) = foldl (<>) (g v <> g' obj) (fmap (g' . snd) vs)
  g' v@(ObjectUpdateNested _ obj vs) = foldl (<>) (g v <> g' obj) (fmap g' vs)
  g' v@(Abs _ b v1) = g v <> h' b <> g' v1
  g' v@(App _ v1 v2) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse _ v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case _ vs alts) = foldl (<>) (foldl (<>) (g v) (fmap g' vs)) (fmap i' alts)
  g' v@(TypedValue _ _ v1 _) = g v <> g' v1
  g' v@(Let _ ds v1) = foldl (<>) (g v) (fmap f' ds) <> g' v1
  g' v@(Do _ es) = foldl (<>) (g v) (fmap j' es)
  g' v = g v

  h' :: Binder -> r
  h' b@(LiteralBinder _ l) = lit (h b) h' l
  h' b@(ConstructorBinder _ _ bs) = foldl (<>) (h b) (fmap h' bs)
  h' b@(BinaryNoParensBinder _ b1 b2 b3) = h b <> h' b1 <> h' b2 <> h' b3
  h' b@(ParensInBinder _ b1) = h b <> h' b1
  h' b@(NamedBinder _ _ b1) = h b <> h' b1
  h' b@(TypedBinder _ _ b1) = h b <> h' b1
  h' b = h b

  lit :: r -> (a -> r) -> Literal a -> r
  lit r go (ArrayLiteral as) = foldl (<>) r (fmap go as)
  lit r go (ObjectLiteral as) = foldl (<>) r (fmap (go . snd) as)
  lit r _ _ = r

  i' :: CaseAlternative -> r
  i' ca@(CaseAlternative _ bs gs) =
    foldl (<>) (i ca) (fmap h' bs ++ concatMap (\(GuardedExpr _ grd val) -> fmap k' grd ++ [g' val]) gs)

  j' :: DoNotationElement -> r
  j' e@(DoNotationValue _ v) = j e <> g' v
  j' e@(DoNotationBind _ b v) = j e <> h' b <> g' v
  j' e@(DoNotationLet _ ds) = foldl (<>) (j e) (fmap f' ds)

  k' :: Guard -> r
  k' (ConditionGuard _ e) = g' e
  k' (PatternGuard _ b e) = h' b <> g' e

everythingWithContextOnValues
  :: forall s r
   . s
  -> r
  -> (r -> r -> r)
  -> (s -> Declaration       -> (s, r))
  -> (s -> Expr              -> (s, r))
  -> (s -> Binder            -> (s, r))
  -> (s -> CaseAlternative   -> (s, r))
  -> (s -> DoNotationElement -> (s, r))
  -> ( Declaration       -> r
     , Expr              -> r
     , Binder            -> r
     , CaseAlternative   -> r
     , DoNotationElement -> r)
everythingWithContextOnValues s0 r0 (<>) f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where

  f'' :: s -> Declaration -> r
  f'' s d = let (s', r) = f s d in r <> f' s' d

  f' :: s -> Declaration -> r
  f' s (DataBindingGroupDeclaration ds) = foldl (<>) r0 (fmap (f'' s) ds)
  f' s (ValueDeclaration _ _ _ bs val) = foldl (<>) r0 (fmap (h'' s) bs ++ concatMap (\(GuardedExpr _ grd v) -> fmap (k' s) grd ++ [g'' s v]) val)
  f' s (BindingGroupDeclaration ds) = foldl (<>) r0 (fmap (\(_, _, val) -> g'' s val) ds)
  f' s (TypeClassDeclaration _ _ _ _ _ ds) = foldl (<>) r0 (fmap (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ _ (ExplicitInstance ds)) = foldl (<>) r0 (fmap (f'' s) ds)
  f' _ _ = r0

  g'' :: s -> Expr -> r
  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' :: s -> Expr -> r
  g' s (Literal _ l) = lit g'' s l
  g' s (UnaryMinus _ v1) = g'' s v1
  g' s (BinaryNoParens _ op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens _ v1) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ _ v1) = g'' s v1
  g' s (Accessor _ _ v1) = g'' s v1
  g' s (ObjectUpdate _ obj vs) = foldl (<>) (g'' s obj) (fmap (g'' s . snd) vs)
  g' s (ObjectUpdateNested _ obj vs) = foldl (<>) (g'' s obj) (fmap (g'' s) vs)
  g' s (Abs _ binder v1) = h'' s binder <> g'' s v1
  g' s (App _ v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse _ v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case _ vs alts) = foldl (<>) (foldl (<>) r0 (fmap (g'' s) vs)) (fmap (i'' s) alts)
  g' s (TypedValue _ _ v1 _) = g'' s v1
  g' s (Let _ ds v1) = foldl (<>) r0 (fmap (f'' s) ds) <> g'' s v1
  g' s (Do _ es) = foldl (<>) r0 (fmap (j'' s) es)
  g' _ _ = r0

  h'' :: s -> Binder -> r
  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' :: s -> Binder -> r
  h' s (LiteralBinder _ l) = lit h'' s l
  h' s (ConstructorBinder _ _ bs) = foldl (<>) r0 (fmap (h'' s) bs)
  h' s (BinaryNoParensBinder _ b1 b2 b3) = h'' s b1 <> h'' s b2 <> h'' s b3
  h' s (ParensInBinder _ b) = h'' s b
  h' s (NamedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ _ b1) = h'' s b1
  h' _ _ = r0

  lit :: (s -> a -> r) -> s -> Literal a -> r
  lit go s (ArrayLiteral as) = foldl (<>) r0 (fmap (go s) as)
  lit go s (ObjectLiteral as) = foldl (<>) r0 (fmap (go s . snd) as)
  lit _ _ _ = r0

  i'' :: s -> CaseAlternative -> r
  i'' s ca = let (s', r) = i s ca in r <> i' s' ca

  i' :: s -> CaseAlternative -> r
  i' s (CaseAlternative _ bs gs) = foldl (<>) r0 (fmap (h'' s) bs ++ concatMap (\(GuardedExpr _ grd val) -> fmap (k' s) grd ++ [g'' s val]) gs)

  j'' :: s -> DoNotationElement -> r
  j'' s e = let (s', r) = j s e in r <> j' s' e

  j' :: s -> DoNotationElement -> r
  j' s (DoNotationValue _ v) = g'' s v
  j' s (DoNotationBind _ b v) = h'' s b <> g'' s v
  j' s (DoNotationLet _ ds) = foldl (<>) r0 (fmap (f'' s) ds)

  k' :: s -> Guard -> r
  k' s (ConditionGuard _ e) = g'' s e
  k' s (PatternGuard _ b e) = h'' s b <> g'' s e

everywhereWithContextOnValuesM
  :: forall m s
   . (Monad m)
  => s
  -> (s -> Declaration       -> m (s, Declaration))
  -> (s -> Expr              -> m (s, Expr))
  -> (s -> Binder            -> m (s, Binder))
  -> (s -> CaseAlternative   -> m (s, CaseAlternative))
  -> (s -> DoNotationElement -> m (s, DoNotationElement))
  -> ( Declaration       -> m Declaration
     , Expr              -> m Expr
     , Binder            -> m Binder
     , CaseAlternative   -> m CaseAlternative
     , DoNotationElement -> m DoNotationElement
     )
everywhereWithContextOnValuesM s0 f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f'' s) ds
  f' s (ValueDeclaration sa name nameKind bs val) = ValueDeclaration sa name nameKind <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val
  f' s (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration sa name args implies deps ds) = TypeClassDeclaration sa name args implies deps <$> traverse (f'' s) ds
  f' s (TypeInstanceDeclaration sa name cs className args ds) = TypeInstanceDeclaration sa name cs className args <$> traverseTypeInstanceBody (traverse (f'' s)) ds
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (Literal ss l) = Literal ss <$> lit g'' s l
  g' s (UnaryMinus ss v) = UnaryMinus ss <$> g'' s v
  g' s (BinaryNoParens ss op v1 v2) = BinaryNoParens ss <$> g'' s op <*> g'' s v1 <*> g'' s v2
  g' s (Parens ss v) = Parens ss <$> g'' s v
  g' s (TypeClassDictionaryConstructorApp ss name v) = TypeClassDictionaryConstructorApp ss name <$> g'' s v
  g' s (Accessor ss prop v) = Accessor ss prop <$> g'' s v
  g' s (ObjectUpdate ss obj vs) = ObjectUpdate ss <$> g'' s obj <*> traverse (sndM (g'' s)) vs
  g' s (ObjectUpdateNested ss obj vs) = ObjectUpdateNested ss <$> g'' s obj <*> traverse (g'' s) vs
  g' s (Abs ss binder v) = Abs ss <$> h' s binder <*> g'' s v
  g' s (App ss v1 v2) = App ss <$> g'' s v1 <*> g'' s v2
  g' s (IfThenElse ss v1 v2 v3) = IfThenElse ss <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case ss vs alts) = Case ss <$> traverse (g'' s) vs <*> traverse (i'' s) alts
  g' s (TypedValue ss check v ty) = TypedValue ss check <$> g'' s v <*> pure ty
  g' s (Let ss ds v) = Let ss <$> traverse (f'' s) ds <*> g'' s v
  g' s (Do ss es) = Do ss <$> traverse (j'' s) es
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (LiteralBinder ss l) = LiteralBinder ss <$> lit h'' s l
  h' s (ConstructorBinder ss ctor bs) = ConstructorBinder ss ctor <$> traverse (h'' s) bs
  h' s (BinaryNoParensBinder ss b1 b2 b3) = BinaryNoParensBinder ss <$> h'' s b1 <*> h'' s b2 <*> h'' s b3
  h' s (ParensInBinder ss b) = ParensInBinder ss <$> h'' s b
  h' s (NamedBinder ss name b) = NamedBinder ss name <$> h'' s b
  h' s (TypedBinder ss t b) = TypedBinder ss t <$> h'' s b
  h' _ other = return other

  lit :: (s -> a -> m a) -> s -> Literal a -> m (Literal a)
  lit go s (ArrayLiteral as) = ArrayLiteral <$> traverse (go s) as
  lit go s (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM (go s)) as
  lit _ _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative ss bs val) = CaseAlternative ss <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue ss v) = DoNotationValue ss <$> g'' s v
  j' s (DoNotationBind ss b v) = DoNotationBind ss <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ss ds) = DoNotationLet ss <$> traverse (f'' s) ds

  k' s (ConditionGuard ss e) = ConditionGuard ss <$> g'' s e
  k' s (PatternGuard ss b e) = PatternGuard ss <$> h'' s b <*> g'' s e

everythingWithScope
  :: forall r
   . (Monoid r)
  => (S.Set Ident -> Declaration -> r)
  -> (S.Set Ident -> Expr -> r)
  -> (S.Set Ident -> Binder -> r)
  -> (S.Set Ident -> CaseAlternative -> r)
  -> (S.Set Ident -> DoNotationElement -> r)
  -> ( S.Set Ident -> Declaration -> r
     , S.Set Ident -> Expr -> r
     , S.Set Ident -> Binder -> r
     , S.Set Ident -> CaseAlternative -> r
     , S.Set Ident -> DoNotationElement -> r
     )
everythingWithScope f g h i j = (f'', g'', h'', i'', \s -> snd . j'' s)
  where
  -- Avoid importing Data.Monoid and getting shadowed names above
  (<>) = mappend

  f'' :: S.Set Ident -> Declaration -> r
  f'' s a = f s a <> f' s a

  f' :: S.Set Ident -> Declaration -> r
  f' s (DataBindingGroupDeclaration ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent (NEL.toList ds)))
    in foldMap (f'' s') ds
  f' s (ValueDeclaration _ name _ bs val) =
    let s' = S.insert name s
        s'' = S.union s' (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s') bs <> foldMap (l' s'') val
  f' s (BindingGroupDeclaration ds) =
    let s' = S.union s (S.fromList (NEL.toList (fmap (\((_, name), _, _) -> name) ds)))
    in foldMap (\(_, _, val) -> g'' s' val) ds
  f' s (TypeClassDeclaration _ _ _ _ _ ds) = foldMap (f'' s) ds
  f' s (TypeInstanceDeclaration _ _ _ _ _ (ExplicitInstance ds)) = foldMap (f'' s) ds
  f' _ _ = mempty

  g'' :: S.Set Ident -> Expr -> r
  g'' s a = g s a <> g' s a

  g' :: S.Set Ident -> Expr -> r
  g' s (Literal _ l) = lit g'' s l
  g' s (UnaryMinus _ v1) = g'' s v1
  g' s (BinaryNoParens _ op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens _ v1) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ _ v1) = g'' s v1
  g' s (Accessor _ _ v1) = g'' s v1
  g' s (ObjectUpdate _ obj vs) = g'' s obj <> foldMap (g'' s . snd) vs
  g' s (ObjectUpdateNested _ obj vs) = g'' s obj <> foldMap (g'' s) vs
  g' s (Abs _ b v1) =
    let s' = S.union (S.fromList (binderNames b)) s
    in h'' s b <> g'' s' v1
  g' s (App _ v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse _ v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case _ vs alts) = foldMap (g'' s) vs <> foldMap (i'' s) alts
  g' s (TypedValue _ _ v1 _) = g'' s v1
  g' s (Let _ ds v1) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds <> g'' s' v1
  g' s (Do _ es) = fold . snd . mapAccumL j'' s $ es
  g' _ _ = mempty

  h'' :: S.Set Ident -> Binder -> r
  h'' s a = h s a <> h' s a

  h' :: S.Set Ident -> Binder -> r
  h' s (LiteralBinder _ l) = lit h'' s l
  h' s (ConstructorBinder _ _ bs) = foldMap (h'' s) bs
  h' s (BinaryNoParensBinder _ b1 b2 b3) = foldMap (h'' s) [b1, b2, b3]
  h' s (ParensInBinder _ b) = h'' s b
  h' s (NamedBinder _ name b1) = h'' (S.insert name s) b1
  h' s (TypedBinder _ _ b1) = h'' s b1
  h' _ _ = mempty

  lit :: (S.Set Ident -> a -> r) -> S.Set Ident -> Literal a -> r
  lit go s (ArrayLiteral as) = foldMap (go s) as
  lit go s (ObjectLiteral as) = foldMap (go s . snd) as
  lit _ _ _ = mempty

  i'' :: S.Set Ident -> CaseAlternative -> r
  i'' s a = i s a <> i' s a

  i' :: S.Set Ident -> CaseAlternative -> r
  i' s (CaseAlternative _ bs gs) =
    let s' = S.union s (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s) bs <> foldMap (l' s') gs

  j'' :: S.Set Ident -> DoNotationElement -> (S.Set Ident, r)
  j'' s a = let (s', r) = j' s a in (s', j s a <> r)

  j' :: S.Set Ident -> DoNotationElement -> (S.Set Ident, r)
  j' s (DoNotationValue _ v) = (s, g'' s v)
  j' s (DoNotationBind _ b v) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s v)
  j' s (DoNotationLet _ ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in (s', foldMap (f'' s') ds)

  k' :: S.Set Ident -> Guard -> (S.Set Ident, r)
  k' s (ConditionGuard _ e) = (s, g'' s e)
  k' s (PatternGuard _ b e) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s' e)

  l' s (GuardedExpr _ [] e) = g'' s e
  l' s (GuardedExpr ss (grd:gs) e) =
    let (s', r) = k' s grd
    in r <> l' s' (GuardedExpr ss gs e)

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (ValueDeclaration _ ident _ _ _) = Just ident
  getDeclIdent (TypeDeclaration _ ident _) = Just ident
  getDeclIdent _ = Nothing

accumTypes
  :: (Monoid r)
  => (Type -> r)
  -> ( Declaration -> r
     , Expr -> r
     , Binder -> r
     , CaseAlternative -> r
     , DoNotationElement -> r
     )
accumTypes f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ _ dctors) = mconcat (concatMap (fmap f . snd) dctors)
  forDecls (ExternDeclaration _ _ ty) = f ty
  forDecls (TypeClassDeclaration _ _ _ implies _ _) = mconcat (concatMap (fmap f . constraintArgs) implies)
  forDecls (TypeInstanceDeclaration _ _ cs _ tys _) = mconcat (concatMap (fmap f . constraintArgs) cs) `mappend` mconcat (fmap f tys)
  forDecls (TypeSynonymDeclaration _ _ _ ty) = f ty
  forDecls (TypeDeclaration _ _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary _ c _ _) = mconcat (fmap f (constraintArgs c))
  forValues (DeferredDictionary _ _ tys) = mconcat (fmap f tys)
  forValues (TypedValue _ _ _ ty) = f ty
  forValues _ = mempty

accumKinds
  :: (Monoid r)
  => (Kind -> r)
  -> ( Declaration -> r
     , Expr -> r
     , Binder -> r
     , CaseAlternative -> r
     , DoNotationElement -> r
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

  forTypes (KindedType _ k) = f k
  forTypes _ = mempty

-- |
-- Map a function over type annotations appearing inside a value
--
overTypes :: (Type -> Type) -> Expr -> Expr
overTypes f = let (_, f', _) = everywhereOnValues id g id in f'
  where
  g :: Expr -> Expr
  g (TypedValue ss checkTy val t) = TypedValue ss checkTy val (f t)
  g (TypeClassDictionary ss c sco hints) = TypeClassDictionary ss (mapConstraintArgs (fmap f) c) sco hints
  g other = other
