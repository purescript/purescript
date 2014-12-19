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
  let imports = nub $ mapMaybe importToCoreFn decls
      exps' = nub $ concatMap exportToCoreFn exps
      externs = nub $ mapMaybe externToCoreFn decls
      decls' = concatMap (declToCoreFn env Nothing) decls
  in Module mn imports exps' externs decls'
moduleToCoreFn _ (A.Module{}) =
  error "Module exports were not elaborated before moduleToCoreFn"

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
declToCoreFn :: Environment -> Maybe SourcePos -> A.Declaration -> [Bind Ann]
declToCoreFn _ sp (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
  [NonRec (properToIdent ctor) $
    Abs (sp, Nothing, Just IsNewtype) (Ident "x") (Var nullAnn $ Qualified Nothing (Ident "x"))]
declToCoreFn _ _ d@(A.DataDeclaration Newtype _ _ _) =
  error $ "Found newtype with multiple constructors: " ++ show d
declToCoreFn _ sp (A.DataDeclaration Data tyName _ ctors) =
  flip map ctors $ \(ctor, tys) ->
    NonRec (properToIdent ctor) $ Constructor (sp, Nothing, Nothing) tyName ctor (length tys)
declToCoreFn env sp (A.DataBindingGroupDeclaration ds) = concatMap (declToCoreFn env sp) ds
declToCoreFn env sp (A.ValueDeclaration name _ _ (Right e)) =
  [NonRec name (exprToCoreFn env sp Nothing e)]
declToCoreFn env sp (A.BindingGroupDeclaration ds) =
  [Rec $ map (\(name, _, e) -> (name, exprToCoreFn env sp Nothing e)) ds]
declToCoreFn _ sp (A.TypeClassDeclaration name _ supers members) =
  [NonRec (properToIdent name) $ mkTypeClassConstructor sp supers members]
declToCoreFn env _ (A.PositionedDeclaration sp d) =
  declToCoreFn env (Just sp) d
declToCoreFn _ _ _ = []

-- |
-- Makes a typeclass dictionary constructor function. The returned expression
-- is a function that accepts the superclass instances and member
-- implementations and returns a record for the instance dictionary.
--
mkTypeClassConstructor :: Maybe SourcePos -> [Constraint] -> [A.Declaration] -> Expr Ann
mkTypeClassConstructor sp supers members =
  let props = [ (arg, Accessor nullAnn arg (Var nullAnn $ Qualified Nothing (Ident "dict"))) | arg <- args ]
  in Abs (sp, Nothing, Just IsTypeClassConstructor)
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
    [ CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral True]
                      (Right $ exprToCoreFn env Nothing Nothing v2)
    , CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral False]
                      (Right $ exprToCoreFn env Nothing Nothing v3) ]
exprToCoreFn env sp ty (A.Constructor name) =
  Var (sp, ty, Just $ getConstructorMeta env name) $ fmap properToIdent name
exprToCoreFn env sp ty (A.Case vs alts) =
  Case (sp, ty, Nothing) (map (exprToCoreFn env sp Nothing) vs) (map (altToCoreFn env sp) alts)
exprToCoreFn env sp _ (A.TypedValue _ v ty) =
  exprToCoreFn env sp (Just ty) v
exprToCoreFn env sp ty (A.Let ds v) =
  Let (sp, ty, Nothing) (concatMap (declToCoreFn env sp) ds) (exprToCoreFn env sp Nothing v)
exprToCoreFn env sp ty (A.TypeClassDictionaryConstructorApp name v) =
  App (sp, ty, Nothing) (Var (Nothing, Nothing, Just IsTypeClassConstructor) $ fmap properToIdent name) (exprToCoreFn env sp Nothing v)
exprToCoreFn env _ ty (A.PositionedValue sp v) =
  exprToCoreFn env (Just sp) ty v
exprToCoreFn _ _ _ e =
  error $ "Unexpected value in exprToCoreFn: " ++ show e

-- |
-- Desugars case alternatives from AST to CoreFn representation.
--
altToCoreFn :: Environment -> Maybe SourcePos -> A.CaseAlternative -> CaseAlternative Ann
altToCoreFn env sp (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn env sp) bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
  go (Left ges) = Left $ map (exprToCoreFn env sp Nothing *** exprToCoreFn env sp Nothing) ges
  go (Right e) = Right (exprToCoreFn env sp Nothing e)

-- |
-- Desugars case binders from AST to CoreFn representation.
--
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
  in ConstructorBinder (sp, Nothing, Just $ getConstructorMeta env dctor) (Qualified mn tctor) dctor (map (binderToCoreFn env sp) bs)
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
