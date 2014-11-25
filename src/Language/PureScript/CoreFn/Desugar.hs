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

import Data.List (sort, nub)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M

import Control.Arrow (second, (***))

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Module
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Types
import qualified Language.PureScript.AST as A
import qualified Language.PureScript.Constants as C

moduleToCoreFn :: Environment -> A.Module -> Module
moduleToCoreFn env (A.Module mn decls (Just exps)) =
  let decls' = concatMap go decls
      imports = mapMaybe goImports decls
      externs = nub $ mapMaybe goExterns decls
      exps' = concatMap goExports exps
  in Module mn imports exps' externs decls'
  where

  go :: A.Declaration -> [Bind]
  go (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
    [NotRec (properToIdent ctor) $
      Meta IsNewtype (Abs (Ident "x") (Var $ Qualified Nothing (Ident "x")))]
  go d@(A.DataDeclaration Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  go (A.DataDeclaration Data tyName _ ctors) =
    flip map ctors $ \(ctor, tys) ->
      NotRec (properToIdent ctor) $ Constructor tyName ctor (length tys)
  go (A.DataBindingGroupDeclaration ds) = concatMap go ds
  go (A.TypeSynonymDeclaration{}) = []
  go d@(A.ValueDeclaration{}) = [declToCoreFn env d]
  go d@(A.BindingGroupDeclaration{}) = [declToCoreFn env d]
  go (A.ExternDeclaration{}) = []
  go (A.ExternDataDeclaration{}) = []
  go (A.ExternInstanceDeclaration{}) = []
  go (A.FixityDeclaration{}) = []
  go (A.ImportDeclaration{}) = []
  go (A.TypeClassDeclaration name _ supers members) =
    let props = [ (arg, Accessor arg (Var $ Qualified Nothing (Ident "dict"))) | arg <- args ]
    in [NotRec (properToIdent name) $
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

  goImports :: A.Declaration -> Maybe ModuleName
  goImports (A.ImportDeclaration name _ _) = Just name
  goImports (A.PositionedDeclaration _ d) = goImports d
  goImports _ = Nothing

  goExterns :: A.Declaration -> Maybe ForeignDecl
  goExterns (A.ExternDeclaration _ name js ty) = Just (name, js, ty)
  goExterns (A.ExternInstanceDeclaration name _ _ _) = Just (name, Nothing, tyObject) -- TODO: needs a type
  goExterns (A.PositionedDeclaration _ d) = goExterns d
  goExterns _ = Nothing

  goExports :: A.DeclarationRef -> [Ident]
  goExports (A.TypeRef _ (Just dctors)) = map properToIdent dctors
  goExports (A.ValueRef name) = [name]
  goExports (A.TypeClassRef name) = [properToIdent name]
  goExports (A.TypeInstanceRef name) = [name]
  goExports (A.PositionedDeclarationRef _ d) = goExports d
  goExports _ = []

moduleToCoreFn _ (A.Module{}) =
  error "Module exports were not elaborated before moduleToCoreFn"

exprToCoreFn :: Environment -> A.Expr -> Expr
exprToCoreFn _ (A.NumericLiteral v) = Literal (NumericLiteral v)
exprToCoreFn _ (A.StringLiteral v) = Literal (StringLiteral v)
exprToCoreFn _ (A.BooleanLiteral v) = Literal (BooleanLiteral v)
exprToCoreFn env (A.ArrayLiteral vs) = Literal (ArrayLiteral $ map (exprToCoreFn env) vs)
exprToCoreFn env (A.ObjectLiteral vs) = Literal (ObjectLiteral $ map (second (exprToCoreFn env)) vs)
exprToCoreFn env (A.Accessor name v) = Accessor name (exprToCoreFn env v)
exprToCoreFn env (A.ObjectUpdate obj vs) =
  ObjectUpdate (exprToCoreFn env obj) $ map (second (exprToCoreFn env)) vs
exprToCoreFn env (A.Abs (Left name) v) = Abs name (exprToCoreFn env v)
exprToCoreFn _ (A.Abs _ _) = error "Abs with Binder argument was not desugared before exprToCoreFn"
exprToCoreFn env (A.App v1 v2) = App (exprToCoreFn env v1) (exprToCoreFn env v2)
exprToCoreFn _ (A.Var ident) = Var ident
exprToCoreFn env (A.IfThenElse v1 v2 v3) =
  Case [exprToCoreFn env v1]
    [ CaseAlternative [LiteralBinder $ BooleanLiteral True] (Right $ exprToCoreFn env v2)
    , CaseAlternative [LiteralBinder $ BooleanLiteral False] (Right $ exprToCoreFn env v3) ]
exprToCoreFn env (A.Constructor name) =
  let ctorType = if isNewtypeConstructor env name then IsNewtype else IsConstructor
  in Meta ctorType (Var $ fmap properToIdent name)
exprToCoreFn env (A.Case vs alts) = Case (map (exprToCoreFn env) vs) (map (altToCoreFn env) alts)
exprToCoreFn env (A.TypedValue _ v ty) = TypedValue (exprToCoreFn env v) ty
exprToCoreFn env (A.Let ds v) = Let (map (declToCoreFn env) ds) (exprToCoreFn env v)
exprToCoreFn env (A.TypeClassDictionaryConstructorApp name v) =
  App (Meta IsTypeClassDictionaryConstructor (Var $ fmap properToIdent name)) (exprToCoreFn env v)
exprToCoreFn env (A.PositionedValue _ v) = exprToCoreFn env v
exprToCoreFn _ e = error $ "Unexpected value in exprToCoreFn: " ++ show e

altToCoreFn :: Environment -> A.CaseAlternative -> CaseAlternative
altToCoreFn env (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn env) bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard, Expr)] Expr
  go (Left ges) = Left $ map (exprToCoreFn env *** exprToCoreFn env) ges
  go (Right e) = Right (exprToCoreFn env e)

binderToCoreFn :: Environment -> A.Binder -> Binder
binderToCoreFn _ (A.NullBinder) = NullBinder
binderToCoreFn _ (A.BooleanBinder b) = LiteralBinder (BooleanLiteral b)
binderToCoreFn _ (A.StringBinder s) = LiteralBinder (StringLiteral s)
binderToCoreFn _ (A.NumberBinder n) = LiteralBinder (NumericLiteral n)
binderToCoreFn _ (A.VarBinder name) = VarBinder name
binderToCoreFn env (A.ConstructorBinder name bs) = ConstructorBinder name (map (binderToCoreFn env) bs)
binderToCoreFn env (A.ObjectBinder bs) = LiteralBinder (ObjectLiteral $ map (second (binderToCoreFn env)) bs)
binderToCoreFn env (A.ArrayBinder bs) = LiteralBinder (ArrayLiteral $ map (binderToCoreFn env) bs)
binderToCoreFn env (A.ConsBinder b1 b2) = ConsBinder (binderToCoreFn env b1) (binderToCoreFn env b2)
binderToCoreFn env (A.NamedBinder name b) = NamedBinder name (binderToCoreFn env b)
binderToCoreFn env (A.PositionedBinder _ b) = binderToCoreFn env b

declToCoreFn :: Environment -> A.Declaration -> Bind
declToCoreFn env (A.ValueDeclaration name _ _ (Right e)) = NotRec name (exprToCoreFn env e)
declToCoreFn env (A.BindingGroupDeclaration ds) = Rec $ map (\(name, _, e) -> (name, exprToCoreFn env e)) ds
declToCoreFn env (A.PositionedDeclaration _ d) = declToCoreFn env d
declToCoreFn _ d = error $ "Unexpected value in declToCoreFn: " ++ show d

-- |
-- Converts a ProperName to an Ident.
--
properToIdent :: ProperName -> Ident
properToIdent = Ident . runProperName

-- |
-- Finds the value stored for a data constructor in the current environment.
-- This is a partial function, but if an invalid type has reached this far then
-- something has gone wrong in typechecking.
--
lookupConstructor :: Environment -> Qualified ProperName -> (DataDeclType, ProperName, Type)
lookupConstructor e ctor = fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors e

-- |
-- Checks whether a data constructor is for a newtype.
--
isNewtypeConstructor :: Environment -> Qualified ProperName -> Bool
isNewtypeConstructor e ctor = case lookupConstructor e ctor of
  (Newtype, _, _) -> True
  (Data, _, _) -> False
