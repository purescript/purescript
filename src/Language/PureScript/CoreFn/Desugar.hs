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
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M

import Control.Arrow (second, (***))

import Language.PureScript.AST.SourcePos
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

type Ann = (Maybe SourcePos, Maybe Type, Maybe Meta)

nullAnn :: Ann
nullAnn = (Nothing, Nothing, Nothing)

moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn env (A.Module mn decls (Just exps)) =
  let decls' = concatMap (go Nothing) decls
      imports = mapMaybe goImports decls
      externs = nub $ mapMaybe goExterns decls
      exps' = concatMap goExports exps
  in Module mn imports exps' externs decls'
  where

  go :: Maybe SourcePos -> A.Declaration -> [Bind Ann]
  go sp (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
    [NonRec (properToIdent ctor) $
      Abs (sp, Nothing, Just IsNewtype) (Ident "x") (Var nullAnn $ Qualified Nothing (Ident "x"))]
  go _ d@(A.DataDeclaration Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  go sp (A.DataDeclaration Data tyName _ ctors) =
    flip map ctors $ \(ctor, tys) ->
      NonRec (properToIdent ctor) $ Constructor (sp, Nothing, Nothing) tyName ctor (length tys)
  go sp (A.DataBindingGroupDeclaration ds) = concatMap (go sp) ds
  go sp d@(A.ValueDeclaration{}) = [declToCoreFn env sp d]
  go sp d@(A.BindingGroupDeclaration{}) = [declToCoreFn env sp d]
  go sp (A.TypeClassDeclaration name _ supers members) =
    let props = [ (arg, Accessor nullAnn arg (Var nullAnn $ Qualified Nothing (Ident "dict"))) | arg <- args ]
    in [NonRec (properToIdent name) $
          Abs (sp, Nothing, Just IsTypeClassDictionaryConstructor) (Ident "dict") (Literal nullAnn $ ObjectLiteral props)]
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
  go _ (A.PositionedDeclaration sp d) = go (Just sp) d
  go _ _ = []

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

exprToCoreFn :: Environment -> Maybe SourcePos -> Maybe Type -> A.Expr -> Expr Ann
exprToCoreFn _ sp ty (A.NumericLiteral v) =
  Literal (sp, ty, Nothing) (NumericLiteral v)
exprToCoreFn _ sp ty (A.StringLiteral v) =
  Literal (sp, ty, Nothing) (StringLiteral v)
exprToCoreFn _ sp ty (A.BooleanLiteral v) =
  Literal (sp, ty, Nothing) (BooleanLiteral v)
exprToCoreFn env sp ty (A.ArrayLiteral vs) =
  Literal (sp, ty, Nothing) (ArrayLiteral $ map (exprToCoreFn env sp Nothing) vs)
exprToCoreFn env sp ty (A.ObjectLiteral vs) =
  Literal (sp, ty, Nothing) (ObjectLiteral $ map (second (exprToCoreFn env sp Nothing)) vs)
exprToCoreFn env sp ty (A.Accessor name v) =
  Accessor (sp, ty, Nothing) name (exprToCoreFn env sp Nothing v)
exprToCoreFn env sp ty (A.ObjectUpdate obj vs) =
  ObjectUpdate (sp, ty, Nothing) (exprToCoreFn env sp Nothing obj) $ map (second (exprToCoreFn env sp Nothing)) vs
exprToCoreFn env sp ty (A.Abs (Left name) v) =
  Abs (sp, ty, Nothing) name (exprToCoreFn env sp Nothing v)
exprToCoreFn _ _ _ (A.Abs _ _) =
  error "Abs with Binder argument was not desugared before exprToCoreFn"
exprToCoreFn env sp ty (A.App v1 v2) =
  App (sp, ty, Nothing) (exprToCoreFn env sp Nothing v1) (exprToCoreFn env sp Nothing v2)
exprToCoreFn _ sp ty (A.Var ident) =
  Var (sp, ty, Nothing) ident
exprToCoreFn env sp ty (A.IfThenElse v1 v2 v3) =
  Case (sp, ty, Nothing) [exprToCoreFn env sp Nothing v1]
    [ CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral True] (Right $ exprToCoreFn env Nothing Nothing v2)
    , CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral False] (Right $ exprToCoreFn env Nothing Nothing v3) ]
exprToCoreFn env sp ty (A.Constructor name) =
  Var (sp, ty, Just $ getConstructorMeta env name) $ fmap properToIdent name
exprToCoreFn env sp ty (A.Case vs alts) =
  Case (sp, ty, Nothing) (map (exprToCoreFn env sp Nothing) vs) (map (altToCoreFn env sp) alts)
exprToCoreFn env sp _ (A.TypedValue _ v ty) =
  exprToCoreFn env sp (Just ty) v
exprToCoreFn env sp ty (A.Let ds v) =
  Let (sp, ty, Nothing) (map (declToCoreFn env sp) ds) (exprToCoreFn env sp Nothing v)
exprToCoreFn env sp ty (A.TypeClassDictionaryConstructorApp name v) =
  App (sp, ty, Nothing) (Var (Nothing, Nothing, Just IsTypeClassDictionaryConstructor) $ fmap properToIdent name) (exprToCoreFn env sp Nothing v)
exprToCoreFn env _ ty (A.PositionedValue sp v) =
  exprToCoreFn env (Just sp) ty v
exprToCoreFn _ _ _ e =
  error $ "Unexpected value in exprToCoreFn: " ++ show e

altToCoreFn :: Environment -> Maybe SourcePos -> A.CaseAlternative -> CaseAlternative Ann
altToCoreFn env sp (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn env sp) bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
  go (Left ges) = Left $ map (exprToCoreFn env sp Nothing *** exprToCoreFn env sp Nothing) ges
  go (Right e) = Right (exprToCoreFn env sp Nothing e)

binderToCoreFn :: Environment -> Maybe SourcePos -> A.Binder -> Binder Ann
binderToCoreFn _ sp (A.NullBinder) =
  NullBinder (sp, Nothing, Nothing)
binderToCoreFn _ sp (A.BooleanBinder b) =
  LiteralBinder (sp, Nothing, Nothing) (BooleanLiteral b)
binderToCoreFn _ sp (A.StringBinder s) =
  LiteralBinder (sp, Nothing, Nothing) (StringLiteral s)
binderToCoreFn _ sp (A.NumberBinder n) =
  LiteralBinder (sp, Nothing, Nothing) (NumericLiteral n)
binderToCoreFn _ sp (A.VarBinder name) =
  VarBinder (sp, Nothing, Nothing) name
binderToCoreFn env sp (A.ConstructorBinder dctor@(Qualified mn _) bs) =
  let (_, tctor, _) = lookupConstructor env dctor
  in ConstructorBinder (sp, Nothing, Nothing) (Qualified mn tctor) dctor (map (binderToCoreFn env sp) bs)
binderToCoreFn env sp (A.ObjectBinder bs) =
  LiteralBinder (sp, Nothing, Nothing) (ObjectLiteral $ map (second (binderToCoreFn env sp)) bs)
binderToCoreFn env sp (A.ArrayBinder bs) =
  LiteralBinder (sp, Nothing, Nothing) (ArrayLiteral $ map (binderToCoreFn env sp) bs)
binderToCoreFn env sp (A.ConsBinder b1 b2) =
  let arrCtor = Qualified (Just $ ModuleName [ProperName "Prim"]) (ProperName "Array")
  in ConstructorBinder (sp, Nothing, Nothing) arrCtor arrCtor $ map (binderToCoreFn env sp) [b1, b2]
binderToCoreFn env sp (A.NamedBinder name b) =
  NamedBinder (sp, Nothing, Nothing) name (binderToCoreFn env sp b)
binderToCoreFn env _ (A.PositionedBinder sp b) =
  binderToCoreFn env (Just sp) b

declToCoreFn :: Environment -> Maybe SourcePos -> A.Declaration -> Bind Ann
declToCoreFn env sp (A.ValueDeclaration name _ _ (Right e)) =
  NonRec name (exprToCoreFn env sp Nothing e)
declToCoreFn env sp (A.BindingGroupDeclaration ds) =
  Rec $ map (\(name, _, e) -> (name, exprToCoreFn env sp Nothing e)) ds
declToCoreFn env _ (A.PositionedDeclaration sp d) =
  declToCoreFn env (Just sp) d
declToCoreFn _ _ d =
  error $ "Unexpected value in declToCoreFn: " ++ show d

-- |
-- Converts a ProperName to an Ident.
--
properToIdent :: ProperName -> Ident
properToIdent = Ident . runProperName

-- |
-- Finds information about data constructors from the current environment.
--
lookupConstructor :: Environment -> Qualified ProperName -> (DataDeclType, ProperName, Type)
lookupConstructor env ctor =
  fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors env

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
  typeConstructor _ = error "Invalid argument to isOnlyConstructor"
