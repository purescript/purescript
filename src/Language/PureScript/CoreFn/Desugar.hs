-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Desugar
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The AST -> CoreFn desugaring step
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Data.List (sort)
import Data.Maybe (mapMaybe)

import Control.Arrow (second, (***))

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Module
import Language.PureScript.Environment
import Language.PureScript.Names
import qualified Language.PureScript.AST as A
import qualified Language.PureScript.Constants as C

moduleToCoreFn :: Environment -> A.Module -> Module
moduleToCoreFn _ (A.Module mn decls (Just exps)) =
  let decls' = concatMap go decls
      imports = filter A.isImportDecl decls
      externs = mapMaybe goExterns decls
      exps' = concatMap goExports exps
  in Module mn imports exps' externs decls'
  where

  go :: A.Declaration -> [Bind]
  go (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
    [NotRec (Ident $ runProperName ctor) $
      Meta IsNewtype (Abs (Ident "x") (Var $ Qualified Nothing (Ident "x")))]
  go d@(A.DataDeclaration Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  go (A.DataDeclaration Data _ _ ctors) =
    flip map ctors $ \(ctor, tys) ->
      let args = [ "value" ++ show index | index <- [0 .. length tys - 1] ]
          props = ("$ctor", Literal (StringLiteral $ runModuleName mn ++ "." ++ runProperName ctor)) : [ (arg, Var $ Qualified Nothing (Ident arg)) | arg <- args ]
      in NotRec (Ident $ runProperName ctor) $
            Meta IsConstructor $
              foldl (\e arg -> Abs (Ident arg) e) (Literal $ ObjectLiteral props) args
  go (A.DataBindingGroupDeclaration ds) = concatMap go ds
  go (A.TypeSynonymDeclaration{}) = []
  go d@(A.ValueDeclaration{}) = [declToCoreFn d]
  go d@(A.BindingGroupDeclaration{}) = [declToCoreFn d]
  go (A.ExternDeclaration{}) = []
  go (A.ExternDataDeclaration{}) = []
  go (A.ExternInstanceDeclaration{}) = []
  go (A.FixityDeclaration{}) = []
  go (A.ImportDeclaration{}) = []
  go (A.TypeClassDeclaration name _ supers members) =
    let props = [ (arg, Accessor arg (Var $ Qualified Nothing (Ident "dict"))) | arg <- args ]
    in [NotRec (Ident $ runProperName name) $
          Meta IsTypeClassDictionaryConstructor $
            Abs (Ident "dict") (Literal $ ObjectLiteral props)]
    where
    args :: [String]
    args = sort $ memberNames ++ superNames
    memberNames :: [String]
    memberNames = memberToName `map` members
    superNames :: [String]
    superNames = [ toSuperName superclass index
                 | (index, (superclass, _)) <- zip [0..] supers
                 ]
    toSuperName :: Qualified ProperName -> Integer -> String
    toSuperName pn index = C.__superclass_ ++ show pn ++ "_" ++ show index
    memberToName :: A.Declaration -> String
    memberToName (A.TypeDeclaration ident _) = runIdent ident
    memberToName (A.PositionedDeclaration _ d) = memberToName d
    memberToName _ = error "Invalid declaration in type class definition"
  go (A.PositionedDeclaration _ d) = go d
  go d = error $ "Unexpected declaration in moduleToCoreFn: " ++ show d

  goExterns :: A.Declaration -> Maybe ForeignDecl
  goExterns (A.ExternDeclaration _ name js ty) = Just (name, js, ty)
  goExterns (A.ExternInstanceDeclaration name _ _ _) = Just (name, Nothing, tyObject) -- TODO: needs a type
  goExterns (A.PositionedDeclaration _ d) = goExterns d
  goExterns _ = Nothing

  goExports :: A.DeclarationRef -> [Ident]
  goExports (A.TypeRef _ (Just dctors)) = map (Ident . runProperName) dctors
  goExports (A.ValueRef name) = [name]
  goExports (A.TypeClassRef name) = [Ident $ runProperName name]
  goExports (A.TypeInstanceRef name) = [name]
  goExports (A.PositionedDeclarationRef _ d) = goExports d
  goExports _ = []

moduleToCoreFn _ (A.Module{}) =
  error "Module exports were not elaborated before moduleToCoreFn"

exprToCoreFn :: A.Expr -> Expr
exprToCoreFn (A.NumericLiteral v) = Literal (NumericLiteral v)
exprToCoreFn (A.StringLiteral v) = Literal (StringLiteral v)
exprToCoreFn (A.BooleanLiteral v) = Literal (BooleanLiteral v)
exprToCoreFn (A.ArrayLiteral vs) = Literal (ArrayLiteral $ map exprToCoreFn vs)
exprToCoreFn (A.ObjectLiteral vs) = Literal (ObjectLiteral $ map (second exprToCoreFn) vs)
exprToCoreFn (A.Accessor name v) = Accessor name (exprToCoreFn v)
exprToCoreFn (A.ObjectUpdate obj vs) =
  ObjectUpdate (exprToCoreFn obj) $ map (second exprToCoreFn) vs
exprToCoreFn (A.Abs (Left name) v) = Abs name (exprToCoreFn v)
exprToCoreFn (A.Abs _ _) = error "Abs with Binder argument was not desugared before exprToCoreFn"
exprToCoreFn (A.App v1 v2) = App (exprToCoreFn v1) (exprToCoreFn v2)
exprToCoreFn (A.Var ident) = Var ident
exprToCoreFn (A.IfThenElse v1 v2 v3) =
  Case [exprToCoreFn v1]
    [ CaseAlternative [LiteralBinder $ BooleanLiteral True] (Right $ exprToCoreFn v2)
    , CaseAlternative [LiteralBinder $ BooleanLiteral False] (Right $ exprToCoreFn v3) ]
exprToCoreFn (A.Constructor name) = Meta IsConstructor (Var $ properToIdent name)
exprToCoreFn (A.Case vs alts) = Case (map exprToCoreFn vs) (map altToCoreFn alts)
exprToCoreFn (A.TypedValue _ v ty) = TypedValue (exprToCoreFn v) ty
exprToCoreFn (A.Let ds v) = Let (map declToCoreFn ds) (exprToCoreFn v)
exprToCoreFn (A.TypeClassDictionaryConstructorApp name v) =
  App (Meta IsTypeClassDictionaryConstructor (Var $ properToIdent name)) (exprToCoreFn v)
exprToCoreFn (A.PositionedValue _ v) = exprToCoreFn v
exprToCoreFn e = error $ "Unexpected value in exprToCoreFn: " ++ show e

altToCoreFn :: A.CaseAlternative -> CaseAlternative
altToCoreFn (A.CaseAlternative bs vs) = CaseAlternative (map binderToCoreFn bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard, Expr)] Expr
  go (Left ges) = Left $ map (exprToCoreFn *** exprToCoreFn) ges
  go (Right e) = Right (exprToCoreFn e)

binderToCoreFn :: A.Binder -> Binder
binderToCoreFn (A.NullBinder) = NullBinder
binderToCoreFn (A.BooleanBinder b) = LiteralBinder (BooleanLiteral b)
binderToCoreFn (A.StringBinder s) = LiteralBinder (StringLiteral s)
binderToCoreFn (A.NumberBinder n) = LiteralBinder (NumericLiteral n)
binderToCoreFn (A.VarBinder name) = VarBinder name
binderToCoreFn (A.ConstructorBinder name bs) = ConstructorBinder name (map binderToCoreFn bs)
binderToCoreFn (A.ObjectBinder bs) = LiteralBinder (ObjectLiteral $ map (second binderToCoreFn) bs)
binderToCoreFn (A.ArrayBinder bs) = LiteralBinder (ArrayLiteral $ map binderToCoreFn bs)
binderToCoreFn (A.ConsBinder b1 b2) = ConsBinder (binderToCoreFn b1) (binderToCoreFn b2)
binderToCoreFn (A.NamedBinder name b) = NamedBinder name (binderToCoreFn b)
binderToCoreFn (A.PositionedBinder _ b) = binderToCoreFn b

declToCoreFn :: A.Declaration -> Bind
declToCoreFn (A.ValueDeclaration name _ _ (Right e)) = NotRec name (exprToCoreFn e)
declToCoreFn (A.BindingGroupDeclaration ds) = Rec $ map (\(name, _, e) -> (name, exprToCoreFn e)) ds
declToCoreFn (A.PositionedDeclaration _ d) = declToCoreFn d
declToCoreFn d = error $ "Unexpected value in declToCoreFn: " ++ show d

properToIdent :: Qualified ProperName -> Qualified Ident
properToIdent (Qualified q name) = Qualified q (Ident $ runProperName name)
