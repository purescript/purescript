module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Prelude.Compat

import Control.Arrow (second, (***))

import Data.Function (on)
import Data.List (sort, sortBy, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Traversals
import Language.PureScript.Comments
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
import qualified Language.PureScript.AST as A

-- |
-- Desugars a module from AST to CoreFn representation.
--
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn _ (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (A.Module _ coms mn decls (Just exps)) =
  let imports = mapMaybe importToCoreFn decls ++ findQualModules decls
      imports' = nub $ filter (keepImp imports) imports-- TODO could be more efficient
      exps' = nub $ concatMap exportToCoreFn exps
      externs = nub $ mapMaybe externToCoreFn decls
      decls' = concatMap (declToCoreFn Nothing []) decls
  in Module coms mn imports' exps' externs decls'

  where

  -- Remove duplicate imports favoring the one containing sourcespan info
  keepImp :: [(Ann, ModuleName)] -> (Ann, ModuleName) -> Bool
  keepImp imps (a, i) = hasSS a || not (any hasDup imps)
    where
      hasDup (a', i') = i == i' && hasSS a'

  hasSS :: Ann -> Bool
  hasSS (Just _, _, _, _) = True
  hasSS _ = False

  ssA :: Maybe SourceSpan -> Ann
  ssA ss = (ss, [], Nothing, Nothing)

  -- |
  -- Desugars member declarations from AST to CoreFn representation.
  --
  declToCoreFn :: Maybe SourceSpan -> [Comment] -> A.Declaration -> [Bind Ann]
  declToCoreFn ss com (A.DataDeclaration Newtype _ _ [(ctor, _)]) =
    [NonRec (ssA ss) (properToIdent ctor) $
      Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var nullAnn $ Qualified Nothing (Ident "x"))]
  declToCoreFn _ _ d@(A.DataDeclaration Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn ss com (A.DataDeclaration Data tyName _ ctors) =
    flip map ctors $ \(ctor, _) ->
      let (_, _, _, fields) = lookupConstructor env (Qualified (Just mn) ctor)
      in NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing, Nothing) tyName ctor fields
  declToCoreFn ss _   (A.DataBindingGroupDeclaration ds) = concatMap (declToCoreFn ss []) ds
  declToCoreFn ss com (A.ValueDeclaration name _ _ (Right e)) =
    [NonRec (ssA ss) name (exprToCoreFn ss com Nothing e)]
  declToCoreFn ss _   (A.BindingGroupDeclaration ds) =
    [Rec $ map (\(name, _, e) -> ((ssA ss, name), exprToCoreFn ss [] Nothing e)) ds]
  declToCoreFn ss com (A.TypeClassDeclaration name _ supers _ members) =
    [NonRec (ssA ss) (properToIdent name) $ mkTypeClassConstructor ss com supers members]
  declToCoreFn _  com (A.PositionedDeclaration ss com1 d) =
    declToCoreFn (Just ss) (com ++ com1) d
  declToCoreFn _ _ _ = []

  -- |
  -- Desugars expressions from AST to CoreFn representation.
  --
  exprToCoreFn :: Maybe SourceSpan -> [Comment] -> Maybe Type -> A.Expr -> Expr Ann
  exprToCoreFn ss com ty (A.Literal lit) =
    Literal (ss, com, ty, Nothing) (fmap (exprToCoreFn ss com Nothing) lit)
  exprToCoreFn ss com ty (A.Accessor name v) =
    Accessor (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.ObjectUpdate obj vs) =
    ObjectUpdate (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing obj) $ map (second (exprToCoreFn ss [] Nothing)) vs
  exprToCoreFn ss com ty (A.Abs (Left name) v) =
    Abs (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ _ _ (A.Abs _ _) =
    internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ss com ty (A.App v1 v2) =
    App (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing v1) (exprToCoreFn ss [] Nothing v2)
  exprToCoreFn ss com ty (A.Var ident) =
    Var (ss, com, ty, getValueMeta ident) ident
  exprToCoreFn ss com ty (A.IfThenElse v1 v2 v3) =
    Case (ss, com, ty, Nothing) [exprToCoreFn ss [] Nothing v1]
      [ CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral True]
                        (Right $ exprToCoreFn Nothing [] Nothing v2)
      , CaseAlternative [LiteralBinder nullAnn $ BooleanLiteral False]
                        (Right $ exprToCoreFn Nothing [] Nothing v3) ]
  exprToCoreFn ss com ty (A.Constructor name) =
    Var (ss, com, ty, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ss com ty (A.Case vs alts) =
    Case (ss, com, ty, Nothing) (map (exprToCoreFn ss [] Nothing) vs) (map (altToCoreFn ss) alts)
  exprToCoreFn ss com _ (A.TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v
  exprToCoreFn ss com ty (A.Let ds v) =
    Let (ss, com, ty, Nothing) (concatMap (declToCoreFn ss []) ds) (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com _  (A.TypeClassDictionaryConstructorApp name (A.TypedValue _ (A.Literal (A.ObjectLiteral vs)) _)) =
    let args = map (exprToCoreFn ss [] Nothing . snd) $ sortBy (compare `on` fst) vs
        ctor = Var (ss, [], Nothing, Just IsTypeClassConstructor) (fmap properToIdent name)
    in foldl (App (ss, com, Nothing, Nothing)) ctor args
  exprToCoreFn ss com ty  (A.TypeClassDictionaryAccessor _ ident) =
    Abs (ss, com, ty, Nothing) (Ident "dict")
      (Accessor nullAnn (runIdent ident) (Var nullAnn $ Qualified Nothing (Ident "dict")))
  exprToCoreFn _ com ty (A.PositionedValue ss com1 v) =
    exprToCoreFn (Just ss) (com ++ com1) ty v
  exprToCoreFn _ _ _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- |
  -- Desugars case alternatives from AST to CoreFn representation.
  --
  altToCoreFn :: Maybe SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
  altToCoreFn ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn ss []) bs) (go vs)
    where
    go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go (Left ges) = Left $ map (exprToCoreFn ss [] Nothing *** exprToCoreFn ss [] Nothing) ges
    go (Right e) = Right (exprToCoreFn ss [] Nothing e)

  -- |
  -- Desugars case binders from AST to CoreFn representation.
  --
  binderToCoreFn :: Maybe SourceSpan -> [Comment] -> A.Binder -> Binder Ann
  binderToCoreFn ss com (A.LiteralBinder lit) =
    LiteralBinder (ss, com, Nothing, Nothing) (fmap (binderToCoreFn ss com) lit)
  binderToCoreFn ss com A.NullBinder =
    NullBinder (ss, com, Nothing, Nothing)
  binderToCoreFn ss com (A.VarBinder name) =
    VarBinder (ss, com, Nothing, Nothing) name
  binderToCoreFn ss com (A.ConstructorBinder dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, com, Nothing, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (map (binderToCoreFn ss []) bs)
  binderToCoreFn ss com (A.NamedBinder name b) =
    NamedBinder (ss, com, Nothing, Nothing) name (binderToCoreFn ss [] b)
  binderToCoreFn _ com (A.PositionedBinder ss com1 b) =
    binderToCoreFn (Just ss) (com ++ com1) b
  binderToCoreFn ss com (A.TypedBinder _ b) =
    binderToCoreFn ss com b
  binderToCoreFn _ _ A.OpBinder{} =
    internalError "OpBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.ParensInBinder{} =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- |
  -- Gets metadata for values.
  --
  getValueMeta :: Qualified Ident -> Maybe Meta
  getValueMeta name =
    case lookupValue env name of
      Just (_, External, _) -> Just IsForeign
      _ -> Nothing

  -- |
  -- Gets metadata for data constructors.
  --
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

-- |
-- Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
--
findQualModules :: [A.Declaration] -> [(Ann, ModuleName)]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
  in map (nullAnn,) $ f `concatMap` decls
  where
  fqDecls :: A.Declaration -> [ModuleName]
  fqDecls (A.TypeInstanceDeclaration _ _ q _ _) = getQual' q
  fqDecls (A.ValueFixityDeclaration _ q _) = getQual' q
  fqDecls (A.TypeFixityDeclaration _ q _) = getQual' q
  fqDecls _ = []

  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var q) = getQual' q
  fqValues (A.Constructor q) = getQual' q
  fqValues _ = []

  fqBinders :: A.Binder -> [ModuleName]
  fqBinders (A.ConstructorBinder q _) = getQual' q
  fqBinders _ = []

  getQual' :: Qualified a -> [ModuleName]
  getQual' = maybe [] return . getQual

-- |
-- Desugars import declarations from AST to CoreFn representation.
--
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
importToCoreFn (A.ImportDeclaration name _ _) = Just (nullAnn, name)
importToCoreFn (A.PositionedDeclaration ss _ d) =
  ((,) (Just ss, [], Nothing, Nothing) . snd) <$> importToCoreFn d
importToCoreFn _ = Nothing

-- |
-- Desugars foreign declarations from AST to CoreFn representation.
--
externToCoreFn :: A.Declaration -> Maybe ForeignDecl
externToCoreFn (A.ExternDeclaration name ty) = Just (name, ty)
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
-- Converts a ProperName to an Ident.
--
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName
