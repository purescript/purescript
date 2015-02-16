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
import Data.List (sort, sortBy, nub)
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
import Language.PureScript.Sugar.TypeClasses (typeClassMemberName, superClassDictionaryNames)
import Language.PureScript.Types
import Language.PureScript.Comments
import qualified Language.PureScript.AST as A

-- |
-- Desugars a module from AST to CoreFn representation.
--
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn env (A.Module mn decls (Just exps)) =
  let imports = nub $ mapMaybe importToCoreFn decls ++ findQualModules decls
      exps' = nub $ concatMap exportToCoreFn exps
      externs = nub $ mapMaybe externToCoreFn decls
      decls' = concatMap (declToCoreFn mn env Nothing []) decls
  in Module mn imports exps' externs decls'
moduleToCoreFn _ (A.Module{}) =
  error "Module exports were not elaborated before moduleToCoreFn"

-- |
-- Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
--
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
importToCoreFn (A.PositionedDeclaration _ _ d) = importToCoreFn d
importToCoreFn _ = Nothing

-- |
-- Desugars foreign declarations from AST to CoreFn representation.
--
externToCoreFn :: A.Declaration -> Maybe ForeignDecl
externToCoreFn (A.ExternDeclaration _ name js ty) = Just (name, js, ty)
externToCoreFn (A.ExternInstanceDeclaration name _ _ _) = Just (name, Nothing, tyObject)
externToCoreFn (A.PositionedDeclaration _ _ d) = externToCoreFn d
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
exportToCoreFn (A.PositionedDeclarationRef _ _ d) = exportToCoreFn d
exportToCoreFn _ = []

-- |
-- Desugars member declarations from AST to CoreFn representation.
--
declToCoreFn :: ModuleName -> Environment -> Maybe SourceSpan -> [Comment] -> A.Declaration -> [Bind Ann]
declToCoreFn _ _ ss com (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
  [NonRec (properToIdent ctor) $
    Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var nullAnn $ Qualified Nothing (Ident "x"))]
declToCoreFn _ _ _ _ d@(A.DataDeclaration Newtype _ _ _) =
  error $ "Found newtype with multiple constructors: " ++ show d
declToCoreFn mn env ss com (A.DataDeclaration Data tyName _ ctors) =
  flip map ctors $ \(ctor, _) ->
    let (_, _, _, fields) = lookupConstructor env (Qualified (Just mn) ctor)
    in NonRec (properToIdent ctor) $ Constructor (ss, com, Nothing, Nothing) tyName ctor fields
declToCoreFn mn env ss _   (A.DataBindingGroupDeclaration ds) = concatMap (declToCoreFn mn env ss []) ds
declToCoreFn mn env ss com (A.ValueDeclaration name _ _ (Right e)) =
  [NonRec name (exprToCoreFn mn env ss com Nothing e)]
declToCoreFn mn env ss _   (A.BindingGroupDeclaration ds) =
  [Rec $ map (\(name, _, e) -> (name, exprToCoreFn mn env ss [] Nothing e)) ds]
declToCoreFn _ _ ss com (A.TypeClassDeclaration name _ supers members) =
  [NonRec (properToIdent name) $ mkTypeClassConstructor ss com supers members]
declToCoreFn mn env _  com (A.PositionedDeclaration ss com1 d) =
  declToCoreFn mn env (Just ss) (com ++ com1) d
declToCoreFn _ _ _ _ _ = []

-- |
-- Makes a typeclass dictionary constructor function. The returned expression
-- is a function that accepts the superclass instances and member
-- implementations and returns a record for the instance dictionary.
--
mkTypeClassConstructor :: Maybe SourceSpan -> [Comment] -> [Constraint] -> [A.Declaration] -> Expr Ann
mkTypeClassConstructor ss com [] [] = Literal (ss, com, Nothing, Just IsTypeClassConstructor) (ObjectLiteral [])
mkTypeClassConstructor ss com supers members =
  let args@(a:as) = sort $ map typeClassMemberName members ++ superClassDictionaryNames supers
      props = [ (arg, Var nullAnn $ Qualified Nothing (Ident arg)) | arg <- args ]
      dict = Literal nullAnn (ObjectLiteral props)
  in Abs (ss, com, Nothing, Just IsTypeClassConstructor)
         (Ident a)
         (foldr (Abs nullAnn . Ident) dict as)

-- |
-- Desugars expressions from AST to CoreFn representation.
--
exprToCoreFn :: ModuleName -> Environment -> Maybe SourceSpan -> [Comment] -> Maybe Type -> A.Expr -> Expr Ann
exprToCoreFn _ _ ss com ty (A.NumericLiteral v) =
  Literal (ss, com, ty, Nothing) (NumericLiteral v)
exprToCoreFn _ _ ss com ty (A.StringLiteral v) =
  Literal (ss, com, ty, Nothing) (StringLiteral v)
exprToCoreFn _ _ ss com ty (A.BooleanLiteral v) =
  Literal (ss, com, ty, Nothing) (BooleanLiteral v)
exprToCoreFn mn env ss com ty (A.ArrayLiteral vs) =
  Literal (ss, com, ty, Nothing) (ArrayLiteral $ map (exprToCoreFn mn env ss [] Nothing) vs)
exprToCoreFn mn env ss com ty (A.ObjectLiteral vs) =
  Literal (ss, com, ty, Nothing) (ObjectLiteral $ map (second (exprToCoreFn mn env ss [] Nothing)) vs)
exprToCoreFn mn env ss com ty (A.Accessor name v) =
  Accessor (ss, com, ty, Nothing) name (exprToCoreFn mn env ss [] Nothing v)
exprToCoreFn mn env ss com ty (A.ObjectUpdate obj vs) =
  ObjectUpdate (ss, com, ty, Nothing) (exprToCoreFn mn env ss [] Nothing obj) $ map (second (exprToCoreFn mn env ss [] Nothing)) vs
exprToCoreFn mn env ss com ty (A.Abs (Left name) v) =
  Abs (ss, com, ty, Nothing) name (exprToCoreFn mn env ss [] Nothing v)
exprToCoreFn _ _ _ _ _ (A.Abs _ _) =
  error "Abs with Binder argument was not desugared before exprToCoreFn mn"
exprToCoreFn mn env ss com ty (A.App v1 v2) =
  App (ss, com, ty, Nothing) (exprToCoreFn mn env ss [] Nothing v1) (exprToCoreFn mn env ss [] Nothing v2)
exprToCoreFn _ _ ss com ty (A.Var ident) =
  Var (ss, com, ty, Nothing) ident
exprToCoreFn mn env ss com ty (A.IfThenElse v1 v2 v3) =
  Case (ss, com, ty, Nothing) [exprToCoreFn mn env ss [] Nothing v1]
    [ CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral True]
                      (Right $ exprToCoreFn mn env Nothing [] Nothing v2)
    , CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral False]
                      (Right $ exprToCoreFn mn env Nothing [] Nothing v3) ]
exprToCoreFn _ env ss com ty (A.Constructor name) =
  Var (ss, com, ty, Just $ getConstructorMeta env name) $ fmap properToIdent name
exprToCoreFn mn env ss com ty (A.Case vs alts) =
  Case (ss, com, ty, Nothing) (map (exprToCoreFn mn env ss [] Nothing) vs) (map (altToCoreFn mn env ss) alts)
exprToCoreFn mn env ss com _ (A.TypedValue _ v ty) =
  exprToCoreFn mn env ss com (Just ty) v
exprToCoreFn mn env ss com ty (A.Let ds v) =
  Let (ss, com, ty, Nothing) (concatMap (declToCoreFn mn env ss []) ds) (exprToCoreFn mn env ss [] Nothing v)
exprToCoreFn mn env ss com _  (A.TypeClassDictionaryConstructorApp name (A.TypedValue _ (A.ObjectLiteral vs) _)) =
  let args = map (exprToCoreFn mn env ss [] Nothing . snd) $ sortBy (compare `on` fst) vs
      ctor = Var (ss, [], Nothing, Just IsTypeClassConstructor) (fmap properToIdent name)
  in foldl (App (ss, com, Nothing, Nothing)) ctor args
exprToCoreFn _ _ ss com ty  (A.TypeClassDictionaryAccessor _ ident) =
  Abs (ss, com, ty, Nothing) (Ident "dict")
    (Accessor nullAnn (runIdent ident) (Var nullAnn $ Qualified Nothing (Ident "dict")))
exprToCoreFn mn env _ com ty (A.PositionedValue ss com1 v) =
  exprToCoreFn mn env (Just ss) (com ++ com1) ty v
exprToCoreFn _ _ _ _ _ e =
  error $ "Unexpected value in exprToCoreFn mn: " ++ show e

-- |
-- Desugars case alternatives from AST to CoreFn representation.
--
altToCoreFn :: ModuleName -> Environment -> Maybe SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
altToCoreFn mn env ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn env ss []) bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
  go (Left ges) = Left $ map (exprToCoreFn mn env ss [] Nothing *** exprToCoreFn mn env ss [] Nothing) ges
  go (Right e) = Right (exprToCoreFn mn env ss [] Nothing e)

-- |
-- Desugars case binders from AST to CoreFn representation.
--
binderToCoreFn :: Environment -> Maybe SourceSpan -> [Comment] -> A.Binder -> Binder Ann
binderToCoreFn _ ss com (A.NullBinder) =
  NullBinder (ss, com, Nothing, Nothing)
binderToCoreFn _ ss com (A.BooleanBinder b) =
  LiteralBinder (ss, com, Nothing, Nothing) (BooleanLiteral b)
binderToCoreFn _ ss com (A.StringBinder s) =
  LiteralBinder (ss, com, Nothing, Nothing) (StringLiteral s)
binderToCoreFn _ ss com (A.NumberBinder n) =
  LiteralBinder (ss, com, Nothing, Nothing) (NumericLiteral n)
binderToCoreFn _ ss com (A.VarBinder name) =
  VarBinder (ss, com, Nothing, Nothing) name
binderToCoreFn env ss com (A.ConstructorBinder dctor@(Qualified mn _) bs) =
  let (_, tctor, _, _) = lookupConstructor env dctor
  in ConstructorBinder (ss, com, Nothing, Just $ getConstructorMeta env dctor) (Qualified mn tctor) dctor (map (binderToCoreFn env ss []) bs)
binderToCoreFn env ss com (A.ObjectBinder bs) =
  LiteralBinder (ss, com, Nothing, Nothing) (ObjectLiteral $ map (second (binderToCoreFn env ss [])) bs)
binderToCoreFn env ss com (A.ArrayBinder bs) =
  LiteralBinder (ss, com, Nothing, Nothing) (ArrayLiteral $ map (binderToCoreFn env ss []) bs)
binderToCoreFn env ss com (A.ConsBinder b1 b2) =
  let arrCtor = Qualified (Just $ ModuleName [ProperName "Prim"]) (ProperName "Array")
  in ConstructorBinder (ss, com, Nothing, Nothing) arrCtor arrCtor $ map (binderToCoreFn env ss []) [b1, b2]
binderToCoreFn env ss com (A.NamedBinder name b) =
  NamedBinder (ss, com, Nothing, Nothing) name (binderToCoreFn env ss [] b)
binderToCoreFn env _ com (A.PositionedBinder ss com1 b) =
  binderToCoreFn env (Just ss) (com ++ com1) b

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
    (Newtype, _, _, _) -> IsNewtype
    dc@(Data, _, _, fields) ->
      let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
      in IsConstructor constructorType fields
  where
  numConstructors :: (Qualified ProperName, (DataDeclType, ProperName, Type, [Ident])) -> Int
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env
  typeConstructor :: (Qualified ProperName, (DataDeclType, ProperName, Type, [Ident])) -> (ModuleName, ProperName)
  typeConstructor (Qualified (Just mn) _, (_, tyCtor, _, _)) = (mn, tyCtor)
  typeConstructor _ = error "Invalid argument to typeConstructor"
