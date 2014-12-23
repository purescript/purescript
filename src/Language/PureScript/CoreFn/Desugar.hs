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

import Data.Function (on)
import Data.List (sort, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Control.Arrow (second, (***))

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Traversals
import Language.PureScript.CoreFn.Ann
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

-- |
-- Desugars a module from AST to CoreFn representation.
--
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn env (A.Module mn decls (Just exps)) =
  let imports = nub $ mapMaybe importToCoreFn decls ++ findQualModules decls
      exps' = nub $ concatMap exportToCoreFn exps
      externs = nub $ mapMaybe externToCoreFn decls
      decls' = concatMap (declToCoreFn env Nothing) decls
  in Module mn imports exps' externs decls'
moduleToCoreFn _ (A.Module{}) =
  error "Module exports were not elaborated before moduleToCoreFn"

findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) (const []) fqValues (const []) (const []) (const [])
  in f `concatMap` decls
  where
  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var (Qualified (Just mn) _)) = [mn]
  fqValues _ = []

-- |
-- Desugars import declarations from AST to CoreFn representation.
--
importToCoreFn :: A.Declaration -> Maybe ModuleName
importToCoreFn (A.ImportDeclaration name _ _) = Just name
importToCoreFn (A.PositionedDeclaration _ d) = importToCoreFn d
importToCoreFn _ = Nothing

-- |
-- Desugars foreign declarations from AST to CoreFn representation.
--
externToCoreFn :: A.Declaration -> Maybe ForeignDecl
externToCoreFn (A.ExternDeclaration _ name js ty) = Just (name, js, ty)
externToCoreFn (A.ExternInstanceDeclaration name _ _ _) = Just (name, Nothing, tyObject)
externToCoreFn (A.PositionedDeclaration _ d) = externToCoreFn d
externToCoreFn _ = Nothing

-- |
-- Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, class
-- constructor, instances and values are flattened into one list.
--
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ (Just dctors)) = map properToIdent dctors
exportToCoreFn (A.ValueRef name) = [name]
exportToCoreFn (A.TypeClassRef name) = [properToIdent name]
exportToCoreFn (A.TypeInstanceRef name) = [name]
exportToCoreFn (A.PositionedDeclarationRef _ d) = exportToCoreFn d
exportToCoreFn _ = []

-- |
-- Desugars member declarations from AST to CoreFn representation.
--
declToCoreFn :: Environment -> Maybe SourceSpan -> A.Declaration -> [Bind Ann]
declToCoreFn _ ss (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
  [NonRec (properToIdent ctor) $
    Abs (ss, Nothing, Just IsNewtype) (Ident "x") (Var nullAnn $ Qualified Nothing (Ident "x"))]
declToCoreFn _ _ d@(A.DataDeclaration Newtype _ _ _) =
  error $ "Found newtype with multiple constructors: " ++ show d
declToCoreFn _ ss (A.DataDeclaration Data tyName _ ctors) =
  flip map ctors $ \(ctor, tys) ->
    NonRec (properToIdent ctor) $ Constructor (ss, Nothing, Nothing) tyName ctor (length tys)
declToCoreFn env ss (A.DataBindingGroupDeclaration ds) = concatMap (declToCoreFn env ss) ds
declToCoreFn env ss (A.ValueDeclaration name _ _ (Right e)) =
  [NonRec name (exprToCoreFn env ss Nothing e)]
declToCoreFn env ss (A.BindingGroupDeclaration ds) =
  [Rec $ map (\(name, _, e) -> (name, exprToCoreFn env ss Nothing e)) ds]
declToCoreFn _ ss (A.TypeClassDeclaration name _ supers members) =
  [NonRec (properToIdent name) $ mkTypeClassConstructor ss supers members]
declToCoreFn env _ (A.PositionedDeclaration ss d) =
  declToCoreFn env (Just ss) d
declToCoreFn _ _ _ = []

-- |
-- Makes a typeclass dictionary constructor function. The returned expression
-- is a function that accepts the superclass instances and member
-- implementations and returns a record for the instance dictionary.
--
mkTypeClassConstructor :: Maybe SourceSpan -> [Constraint] -> [A.Declaration] -> Expr Ann
mkTypeClassConstructor ss supers members =
  let props = [ (arg, Accessor nullAnn arg (Var nullAnn $ Qualified Nothing (Ident "dict"))) | arg <- args ]
  in Abs (ss, Nothing, Just IsTypeClassConstructor)
         (Ident "dict")
         (Literal nullAnn $ ObjectLiteral props)
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

-- |
-- Desugars expressions from AST to CoreFn representation.
--
exprToCoreFn :: Environment -> Maybe SourceSpan -> Maybe Type -> A.Expr -> Expr Ann
exprToCoreFn _ ss ty (A.NumericLiteral v) =
  Literal (ss, ty, Nothing) (NumericLiteral v)
exprToCoreFn _ ss ty (A.StringLiteral v) =
  Literal (ss, ty, Nothing) (StringLiteral v)
exprToCoreFn _ ss ty (A.BooleanLiteral v) =
  Literal (ss, ty, Nothing) (BooleanLiteral v)
exprToCoreFn env ss ty (A.ArrayLiteral vs) =
  Literal (ss, ty, Nothing) (ArrayLiteral $ map (exprToCoreFn env ss Nothing) vs)
exprToCoreFn env ss ty (A.ObjectLiteral vs) =
  Literal (ss, ty, Nothing) (ObjectLiteral $ map (second (exprToCoreFn env ss Nothing)) vs)
exprToCoreFn env ss ty (A.Accessor name v) =
  Accessor (ss, ty, Nothing) name (exprToCoreFn env ss Nothing v)
exprToCoreFn env ss ty (A.ObjectUpdate obj vs) =
  ObjectUpdate (ss, ty, Nothing) (exprToCoreFn env ss Nothing obj) $ map (second (exprToCoreFn env ss Nothing)) vs
exprToCoreFn env ss ty (A.Abs (Left name) v) =
  Abs (ss, ty, Nothing) name (exprToCoreFn env ss Nothing v)
exprToCoreFn _ _ _ (A.Abs _ _) =
  error "Abs with Binder argument was not desugared before exprToCoreFn"
exprToCoreFn env ss ty (A.App v1 v2) =
  App (ss, ty, Nothing) (exprToCoreFn env ss Nothing v1) (exprToCoreFn env ss Nothing v2)
exprToCoreFn _ ss ty (A.Var ident) =
  Var (ss, ty, Nothing) ident
exprToCoreFn env ss ty (A.IfThenElse v1 v2 v3) =
  Case (ss, ty, Nothing) [exprToCoreFn env ss Nothing v1]
    [ CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral True]
                      (Right $ exprToCoreFn env Nothing Nothing v2)
    , CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral False]
                      (Right $ exprToCoreFn env Nothing Nothing v3) ]
exprToCoreFn env ss ty (A.Constructor name) =
  Var (ss, ty, Just $ getConstructorMeta env name) $ fmap properToIdent name
exprToCoreFn env ss ty (A.Case vs alts) =
  Case (ss, ty, Nothing) (map (exprToCoreFn env ss Nothing) vs) (map (altToCoreFn env ss) alts)
exprToCoreFn env ss _ (A.TypedValue _ v ty) =
  exprToCoreFn env ss (Just ty) v
exprToCoreFn env ss ty (A.Let ds v) =
  Let (ss, ty, Nothing) (concatMap (declToCoreFn env ss) ds) (exprToCoreFn env ss Nothing v)
exprToCoreFn env ss ty (A.TypeClassDictionaryConstructorApp name v) =
  App (ss, ty, Nothing) (Var (Nothing, Nothing, Just IsTypeClassConstructor) $ fmap properToIdent name) (exprToCoreFn env ss Nothing v)
exprToCoreFn env _ ty (A.PositionedValue ss v) =
  exprToCoreFn env (Just ss) ty v
exprToCoreFn _ _ _ e =
  error $ "Unexpected value in exprToCoreFn: " ++ show e

-- |
-- Desugars case alternatives from AST to CoreFn representation.
--
altToCoreFn :: Environment -> Maybe SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
altToCoreFn env ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn env ss) bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
  go (Left ges) = Left $ map (exprToCoreFn env ss Nothing *** exprToCoreFn env ss Nothing) ges
  go (Right e) = Right (exprToCoreFn env ss Nothing e)

-- |
-- Desugars case binders from AST to CoreFn representation.
--
binderToCoreFn :: Environment -> Maybe SourceSpan -> A.Binder -> Binder Ann
binderToCoreFn _ ss (A.NullBinder) =
  NullBinder (ss, Nothing, Nothing)
binderToCoreFn _ ss (A.BooleanBinder b) =
  LiteralBinder (ss, Nothing, Nothing) (BooleanLiteral b)
binderToCoreFn _ ss (A.StringBinder s) =
  LiteralBinder (ss, Nothing, Nothing) (StringLiteral s)
binderToCoreFn _ ss (A.NumberBinder n) =
  LiteralBinder (ss, Nothing, Nothing) (NumericLiteral n)
binderToCoreFn _ ss (A.VarBinder name) =
  VarBinder (ss, Nothing, Nothing) name
binderToCoreFn env ss (A.ConstructorBinder dctor@(Qualified mn _) bs) =
  let (_, tctor, _) = lookupConstructor env dctor
  in ConstructorBinder (ss, Nothing, Just $ getConstructorMeta env dctor) (Qualified mn tctor) dctor (map (binderToCoreFn env ss) bs)
binderToCoreFn env ss (A.ObjectBinder bs) =
  LiteralBinder (ss, Nothing, Nothing) (ObjectLiteral $ map (second (binderToCoreFn env ss)) bs)
binderToCoreFn env ss (A.ArrayBinder bs) =
  LiteralBinder (ss, Nothing, Nothing) (ArrayLiteral $ map (binderToCoreFn env ss) bs)
binderToCoreFn env ss (A.ConsBinder b1 b2) =
  let arrCtor = Qualified (Just $ ModuleName [ProperName "Prim"]) (ProperName "Array")
  in ConstructorBinder (ss, Nothing, Nothing) arrCtor arrCtor $ map (binderToCoreFn env ss) [b1, b2]
binderToCoreFn env ss (A.NamedBinder name b) =
  NamedBinder (ss, Nothing, Nothing) name (binderToCoreFn env ss b)
binderToCoreFn env _ (A.PositionedBinder ss b) =
  binderToCoreFn env (Just ss) b

-- |
-- Converts a ProperName to an Ident.
--
properToIdent :: ProperName -> Ident
properToIdent = Ident . runProperName

-- |
-- Gets metadata for data constructors.
--
getConstructorMeta :: Environment -> Qualified ProperName -> Meta
getConstructorMeta env ctor =
  case lookupConstructor env ctor of
    (Newtype, _, _) -> IsNewtype
    dc@(Data, _, ty) ->
      let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
      in IsConstructor constructorType (getArity ty)
  where
  getArity :: Type -> Int
  getArity (TypeApp (TypeApp f _) t) | f == tyFunction = getArity t + 1
  getArity (ForAll _ ty _) = getArity ty
  getArity _ = 0
  numConstructors :: (Qualified ProperName, (DataDeclType, ProperName, Type)) -> Int
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env
  typeConstructor :: (Qualified ProperName, (DataDeclType, ProperName, Type)) -> (ModuleName, ProperName)
  typeConstructor (Qualified (Just mn) _, (_, tyCtor, _)) = (mn, tyCtor)
  typeConstructor _ = error "Invalid argument to typeConstructor"
