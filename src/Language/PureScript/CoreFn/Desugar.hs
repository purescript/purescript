module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow (second)

import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Traversals
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Module
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Sugar.TypeClasses (typeClassMemberName, superClassDictionaryNames)
import Language.PureScript.Types
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.AST as A

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn _ (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (A.Module modSS coms mn decls (Just exps)) =
  let imports = mapMaybe importToCoreFn decls ++ fmap (ssAnn modSS,) (findQualModules decls)
      imports' = dedupeImports imports
      exps' = ordNub $ concatMap exportToCoreFn exps
      externs = ordNub $ mapMaybe externToCoreFn decls
      decls' = concatMap declToCoreFn decls
  in Module coms mn imports' exps' externs decls'

  where

  -- | Remove duplicate imports
  dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
  dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

  ssA :: SourceSpan -> Ann
  ssA ss = (ss, [], Nothing, Nothing)

  -- | Desugars member declarations from AST to CoreFn representation.
  declToCoreFn :: A.Declaration -> [Bind Ann]
  declToCoreFn (A.DataDeclaration (ss, com) Newtype _ _ [(ctor, _)]) =
    [NonRec (ssA ss) (properToIdent ctor) $
      Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var (ssAnn ss) $ Qualified Nothing (Ident "x"))]
  declToCoreFn d@(A.DataDeclaration _ Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn (A.DataDeclaration (ss, com) Data tyName _ ctors) =
    flip fmap ctors $ \(ctor, _) ->
      let (_, _, _, fields) = lookupConstructor env (Qualified (Just mn) ctor)
      in NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing, Nothing) tyName ctor fields
  declToCoreFn (A.DataBindingGroupDeclaration ds) =
    concatMap declToCoreFn ds
  declToCoreFn (A.ValueDecl (ss, _) name _ _ [A.MkUnguarded _ e]) =
    [NonRec (ssA ss) name (exprToCoreFn Nothing e)]
  declToCoreFn (A.BindingGroupDeclaration ds) =
    [Rec . NEL.toList $ fmap (\(((ss, _), name), _, e) -> ((ssA ss, name), exprToCoreFn Nothing e)) ds]
  declToCoreFn (A.TypeClassDeclaration sa@(ss, _) name _ supers _ members) =
    [NonRec (ssA ss) (properToIdent name) $ mkTypeClassConstructor sa supers members]
  declToCoreFn _ = []

  -- | Desugars expressions from AST to CoreFn representation.
  exprToCoreFn :: Maybe Type -> A.Expr -> Expr Ann
  exprToCoreFn ty (A.Literal (ss, com) lit) =
    Literal (ss, com, ty, Nothing) (fmap (exprToCoreFn Nothing) lit)
  exprToCoreFn ty (A.Accessor (ss, com) name v) =
    Accessor (ss, com, ty, Nothing) name (exprToCoreFn Nothing v)
  exprToCoreFn ty (A.ObjectUpdate (ss, com) obj vs) =
    ObjectUpdate (ss, com, ty, Nothing) (exprToCoreFn Nothing obj) $ fmap (second (exprToCoreFn Nothing)) vs
  exprToCoreFn ty (A.Abs (ss, com) (A.VarBinder _ name) v) =
    Abs (ss, com, ty, Nothing) name (exprToCoreFn Nothing v)
  exprToCoreFn _ (A.Abs{}) =
    internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ty (A.App (ss, com) v1 v2) =
    App (ss, com, ty, Nothing) (exprToCoreFn Nothing v1) (exprToCoreFn Nothing v2)
  exprToCoreFn ty (A.Var (ss, com) ident) =
    Var (ss, com, ty, getValueMeta ident) ident
  exprToCoreFn ty (A.IfThenElse (ss, com) v1 v2 v3) =
    Case (ss, com, ty, Nothing) [exprToCoreFn Nothing v1]
      [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                        (Right $ exprToCoreFn Nothing v2)
      , CaseAlternative [NullBinder (ssAnn ss)]
                        (Right $ exprToCoreFn Nothing v3) ]
  exprToCoreFn ty (A.Constructor (ss, com) name) =
    Var (ss, com, ty, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ty (A.Case (ss, com) vs alts) =
    Case (ss, com, ty, Nothing) (fmap (exprToCoreFn Nothing) vs) (fmap altToCoreFn alts)
  exprToCoreFn _ (A.TypedValue _ _ v ty) =
    exprToCoreFn (Just ty) v
  exprToCoreFn ty (A.Let (ss, com) ds v) =
    Let (ss, com, ty, Nothing) (concatMap declToCoreFn ds) (exprToCoreFn Nothing v)
  exprToCoreFn ty (A.TypeClassDictionaryConstructorApp sa name (A.TypedValue _ _ lit@(A.Literal _ (A.ObjectLiteral _)) _)) =
    exprToCoreFn ty (A.TypeClassDictionaryConstructorApp sa name lit)
  exprToCoreFn _ (A.TypeClassDictionaryConstructorApp (ss, com) name (A.Literal _ (A.ObjectLiteral vs))) =
    let args = fmap (exprToCoreFn Nothing . snd) $ sortBy (compare `on` fst) vs
        ctor = Var (ss, [], Nothing, Just IsTypeClassConstructor) (fmap properToIdent name)
    in foldl (App (ss, com, Nothing, Nothing)) ctor args
  exprToCoreFn ty (A.TypeClassDictionaryAccessor (ss, com) _ ident) =
    Abs (ss, com, ty, Nothing) (Ident "dict")
      (Accessor (ssAnn ss) (mkString $ runIdent ident) (Var (ssAnn ss) $ Qualified Nothing (Ident "dict")))
  exprToCoreFn _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- | Desugars case alternatives from AST to CoreFn representation.
  altToCoreFn :: A.CaseAlternative -> CaseAlternative Ann
  altToCoreFn (A.CaseAlternative _ bs vs) = CaseAlternative (map binderToCoreFn bs) (go vs)
    where
    go :: [A.GuardedExpr] -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go [A.MkUnguarded _ e]
      = Right (exprToCoreFn Nothing e)
    go gs
      = Left [ (exprToCoreFn Nothing cond, exprToCoreFn Nothing e)
             | A.GuardedExpr _ g e <- gs
             , let cond = guardToExpr g
             ]

    guardToExpr [A.ConditionGuard _ cond] = cond
    guardToExpr _ = internalError "Guard not correctly desugared"

  -- | Desugars case binders from AST to CoreFn representation.
  binderToCoreFn :: A.Binder -> Binder Ann
  binderToCoreFn (A.LiteralBinder ss lit) =
    LiteralBinder (ss, [], Nothing, Nothing) (fmap binderToCoreFn lit)
  binderToCoreFn (A.NullBinder ss) =
    NullBinder (ss, [], Nothing, Nothing)
  binderToCoreFn (A.VarBinder ss name) =
    VarBinder (ss, [], Nothing, Nothing) name
  binderToCoreFn (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, [], Nothing, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (fmap binderToCoreFn bs)
  binderToCoreFn (A.NamedBinder ss name b) =
    NamedBinder (ss, [], Nothing, Nothing) name (binderToCoreFn b)
  binderToCoreFn (A.TypedBinder _ _ b) =
    binderToCoreFn b
  binderToCoreFn A.OpBinder{} =
    internalError "OpBinder should have been desugared before binderToCoreFn"
  binderToCoreFn A.BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
  binderToCoreFn A.ParensInBinder{} =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- | Gets metadata for values.
  getValueMeta :: Qualified Ident -> Maybe Meta
  getValueMeta name =
    case lookupValue env name of
      Just (_, External, _) -> Just IsForeign
      _ -> Nothing

  -- | Gets metadata for data constructors.
  getConstructorMeta :: Qualified (ProperName 'ConstructorName) -> Meta
  getConstructorMeta ctor =
    case lookupConstructor env ctor of
      (Newtype, _, _, _) -> IsNewtype
      dc@(Data, _, _, fields) ->
        let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
        in IsConstructor constructorType fields
    where

    numConstructors
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, Type, [Ident]))
      -> Int
    numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env

    typeConstructor
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, Type, [Ident]))
      -> (ModuleName, ProperName 'TypeName)
    typeConstructor (Qualified (Just mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
    typeConstructor _ = internalError "Invalid argument to typeConstructor"

-- | Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
  in f `concatMap` decls
  where
  fqDecls :: A.Declaration -> [ModuleName]
  fqDecls (A.TypeInstanceDeclaration _ _ _ q _ _) = getQual' q
  fqDecls (A.ValueFixityDeclaration _ _ q _) = getQual' q
  fqDecls (A.TypeFixityDeclaration _ _ q _) = getQual' q
  fqDecls _ = []

  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var _ q) = getQual' q
  fqValues (A.Constructor _ q) = getQual' q
  -- Some instances are automatically solved and have their class dictionaries
  -- built inline instead of having a named instance defined and imported.
  -- We therefore need to import these constructors if they aren't already.
  fqValues (A.TypeClassDictionaryConstructorApp _ c _) = getQual' c
  fqValues _ = []

  fqBinders :: A.Binder -> [ModuleName]
  fqBinders (A.ConstructorBinder _ q _) = getQual' q
  fqBinders _ = []

  getQual' :: Qualified a -> [ModuleName]
  getQual' = maybe [] return . getQual

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
importToCoreFn (A.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: A.Declaration -> Maybe ForeignDecl
externToCoreFn (A.ExternDeclaration _ name ty) = Just (name, ty)
externToCoreFn _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, class
-- constructor, instances and values are flattened into one list.
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (A.ValueRef _ name) = [name]
exportToCoreFn (A.TypeClassRef _ name) = [properToIdent name]
exportToCoreFn (A.TypeInstanceRef _ name) = [name]
exportToCoreFn _ = []

-- | Makes a typeclass dictionary constructor function. The returned expression
-- is a function that accepts the superclass instances and member
-- implementations and returns a record for the instance dictionary.
mkTypeClassConstructor :: SourceAnn -> [Constraint] -> [A.Declaration] -> Expr Ann
mkTypeClassConstructor (ss, com) [] [] = Literal (ss, com, Nothing, Just IsTypeClassConstructor) (ObjectLiteral [])
mkTypeClassConstructor (ss, com) supers members =
  let args@(a:as) = sort $ fmap typeClassMemberName members ++ superClassDictionaryNames supers
      props = [ (mkString arg, Var (ssAnn ss) $ Qualified Nothing (Ident arg)) | arg <- args ]
      dict = Literal (ssAnn ss) (ObjectLiteral props)
  in Abs (ss, com, Nothing, Just IsTypeClassConstructor)
         (Ident a)
         (foldr (Abs (ssAnn ss) . Ident) dict as)

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName
