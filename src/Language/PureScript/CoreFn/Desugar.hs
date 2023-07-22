module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Prelude
import Protolude (ordNub, orEmpty)

import Control.Arrow (second)

import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceSpan, SourceSpan(..))
import Language.PureScript.AST.Traversals (everythingOnValues)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Ann (Ann, ssAnn)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard)
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), NameKind(..), isDictTypeName, lookupConstructor, lookupValue)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), getQual)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (pattern REmptyKinded, SourceType, Type(..))
import Language.PureScript.AST qualified as A
import Language.PureScript.Constants.Prim qualified as C

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn _ (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (A.Module modSS coms mn decls (Just exps)) =
  let imports = mapMaybe importToCoreFn decls ++ fmap (ssAnn modSS,) (findQualModules decls)
      imports' = dedupeImports imports
      exps' = ordNub $ concatMap exportToCoreFn exps
      reExps = M.map ordNub $ M.unionsWith (++) (mapMaybe (fmap reExportsToCoreFn . toReExportRef) exps)
      externs = ordNub $ mapMaybe externToCoreFn decls
      decls' = concatMap declToCoreFn decls
  in Module modSS coms mn (spanName modSS) imports' exps' reExps externs decls'
  where
  -- Creates a map from a module name to the re-export references defined in
  -- that module.
  reExportsToCoreFn :: (ModuleName, A.DeclarationRef) -> M.Map ModuleName [Ident]
  reExportsToCoreFn (mn', ref') = M.singleton mn' (exportToCoreFn ref')

  toReExportRef :: A.DeclarationRef -> Maybe (ModuleName, A.DeclarationRef)
  toReExportRef (A.ReExportRef _ src ref) =
      fmap
        (, ref)
        (A.exportSourceImportedFrom src)
  toReExportRef _ = Nothing

  -- Remove duplicate imports
  dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
  dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

  ssA :: SourceSpan -> Ann
  ssA ss = (ss, [], Nothing)

  -- Desugars member declarations from AST to CoreFn representation.
  declToCoreFn :: A.Declaration -> [Bind Ann]
  declToCoreFn (A.DataDeclaration (ss, com) Newtype _ _ [ctor]) =
    [NonRec (ss, [], declMeta) (properToIdent $ A.dataCtorName ctor) $
      Abs (ss, com, Just IsNewtype) (Ident "x") (Var (ssAnn ss) $ Qualified ByNullSourcePos (Ident "x"))]
    where
    declMeta = isDictTypeName (A.dataCtorName ctor) `orEmpty` IsTypeClassConstructor
  declToCoreFn d@(A.DataDeclaration _ Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn (A.DataDeclaration (ss, com) Data tyName _ ctors) =
    flip fmap ctors $ \ctorDecl ->
      let
        ctor = A.dataCtorName ctorDecl
        (_, _, _, fields) = lookupConstructor env (Qualified (ByModuleName mn) ctor)
      in NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing) tyName ctor fields
  declToCoreFn (A.DataBindingGroupDeclaration ds) =
    concatMap declToCoreFn ds
  declToCoreFn (A.ValueDecl (ss, com) name _ _ [A.MkUnguarded e]) =
    [NonRec (ssA ss) name (exprToCoreFn ss com Nothing e)]
  declToCoreFn (A.BindingGroupDeclaration ds) =
    [Rec . NEL.toList $ fmap (\(((ss, com), name), _, e) -> ((ssA ss, name), exprToCoreFn ss com Nothing e)) ds]
  declToCoreFn _ = []

  -- Desugars expressions from AST to CoreFn representation.
  exprToCoreFn :: SourceSpan -> [Comment] -> Maybe SourceType -> A.Expr -> Expr Ann
  exprToCoreFn _ com _ (A.Literal ss lit) =
    Literal (ss, com, Nothing) (fmap (exprToCoreFn ss com Nothing) lit)
  exprToCoreFn ss com _ (A.Accessor name v) =
    Accessor (ss, com, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.ObjectUpdate obj vs) =
    ObjectUpdate (ss, com, Nothing) (exprToCoreFn ss [] Nothing obj) (ty >>= unchangedRecordFields (fmap fst vs)) $ fmap (second (exprToCoreFn ss [] Nothing)) vs
    where
    -- Return the unchanged labels of a closed record, or Nothing for other types or open records.
    unchangedRecordFields :: [PSString] -> Type a -> Maybe [PSString]
    unchangedRecordFields updated (TypeApp _ (TypeConstructor _ C.Record) row) =
      collect row
      where
        collect :: Type a -> Maybe [PSString]
        collect (REmptyKinded _ _) = Just []
        collect (RCons _ (Label l) _ r) = (if l `elem` updated then id else (l :)) <$> collect r
        collect _ = Nothing
    unchangedRecordFields _ _ = Nothing
  exprToCoreFn ss com _ (A.Abs (A.VarBinder _ name) v) =
    Abs (ss, com, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ _ _ (A.Abs _ _) =
    internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ss com _ (A.App v1 v2) =
    App (ss, com, (isDictCtor v1 || isSynthetic v2) `orEmpty` IsSyntheticApp) v1' v2'
    where
    v1' = exprToCoreFn ss [] Nothing v1
    v2' = exprToCoreFn ss [] Nothing v2
    isDictCtor = \case
      A.Constructor _ (Qualified _ name) -> isDictTypeName name
      _ -> False
    isSynthetic = \case
      A.App v3 v4            -> isDictCtor v3 || isSynthetic v3 && isSynthetic v4
      A.Accessor _ v3        -> isSynthetic v3
      A.Var NullSourceSpan _ -> True
      A.Unused{}             -> True
      _                      -> False
  exprToCoreFn ss com _ (A.Unused _) =
    Var (ss, com, Nothing) C.I_undefined
  exprToCoreFn _ com _ (A.Var ss ident) =
    Var (ss, com, getValueMeta ident) ident
  exprToCoreFn ss com _ (A.IfThenElse v1 v2 v3) =
    Case (ss, com, Nothing) [exprToCoreFn ss [] Nothing v1]
      [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                        (Right $ exprToCoreFn ss [] Nothing v2)
      , CaseAlternative [NullBinder (ssAnn ss)]
                        (Right $ exprToCoreFn ss [] Nothing v3) ]
  exprToCoreFn _ com _ (A.Constructor ss name) =
    Var (ss, com, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ss com _ (A.Case vs alts) =
    Case (ss, com, Nothing) (fmap (exprToCoreFn ss [] Nothing) vs) (fmap (altToCoreFn ss) alts)
  exprToCoreFn ss com _ (A.TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v
  exprToCoreFn ss com _ (A.Let w ds v) =
    Let (ss, com, getLetMeta w) (concatMap declToCoreFn ds) (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ com ty (A.PositionedValue ss com1 v) =
    exprToCoreFn ss (com ++ com1) ty v
  exprToCoreFn _ _ _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- Desugars case alternatives from AST to CoreFn representation.
  altToCoreFn :: SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
  altToCoreFn ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn ss []) bs) (go vs)
    where
    go :: [A.GuardedExpr] -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go [A.MkUnguarded e]
      = Right (exprToCoreFn ss [] Nothing e)
    go gs
      = Left [ (exprToCoreFn ss [] Nothing cond, exprToCoreFn ss [] Nothing e)
             | A.GuardedExpr g e <- gs
             , let cond = guardToExpr g
             ]

    guardToExpr [A.ConditionGuard cond] = cond
    guardToExpr _ = internalError "Guard not correctly desugared"

  -- Desugars case binders from AST to CoreFn representation.
  binderToCoreFn :: SourceSpan -> [Comment] -> A.Binder -> Binder Ann
  binderToCoreFn _ com (A.LiteralBinder ss lit) =
    LiteralBinder (ss, com, Nothing) (fmap (binderToCoreFn ss com) lit)
  binderToCoreFn ss com A.NullBinder =
    NullBinder (ss, com, Nothing)
  binderToCoreFn _ com (A.VarBinder ss name) =
    VarBinder (ss, com, Nothing) name
  binderToCoreFn _ com (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, com, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (fmap (binderToCoreFn ss []) bs)
  binderToCoreFn _ com (A.NamedBinder ss name b) =
    NamedBinder (ss, com, Nothing) name (binderToCoreFn ss [] b)
  binderToCoreFn _ com (A.PositionedBinder ss com1 b) =
    binderToCoreFn ss (com ++ com1) b
  binderToCoreFn ss com (A.TypedBinder _ b) =
    binderToCoreFn ss com b
  binderToCoreFn _ _ A.OpBinder{} =
    internalError "OpBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.ParensInBinder{} =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- Gets metadata for let bindings.
  getLetMeta :: A.WhereProvenance -> Maybe Meta
  getLetMeta A.FromWhere = Just IsWhere
  getLetMeta A.FromLet = Nothing

  -- Gets metadata for values.
  getValueMeta :: Qualified Ident -> Maybe Meta
  getValueMeta name =
    case lookupValue env name of
      Just (_, External, _) -> Just IsForeign
      _ -> Nothing

  -- Gets metadata for data constructors.
  getConstructorMeta :: Qualified (ProperName 'ConstructorName) -> Meta
  getConstructorMeta ctor =
    case lookupConstructor env ctor of
      (Newtype, _, _, _) -> IsNewtype
      dc@(Data, _, _, fields) ->
        let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
        in IsConstructor constructorType fields
    where

    numConstructors
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
      -> Int
    numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env

    typeConstructor
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
      -> (ModuleName, ProperName 'TypeName)
    typeConstructor (Qualified (ByModuleName mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
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
  fqDecls (A.TypeInstanceDeclaration _ _ _ _ _ _ q _ _) = getQual' q
  fqDecls (A.ValueFixityDeclaration _ _ q _) = getQual' q
  fqDecls (A.TypeFixityDeclaration _ _ q _) = getQual' q
  fqDecls _ = []

  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var _ q) = getQual' q
  fqValues (A.Constructor _ q) = getQual' q
  fqValues _ = []

  fqBinders :: A.Binder -> [ModuleName]
  fqBinders (A.ConstructorBinder _ q _) = getQual' q
  fqBinders _ = []

  getQual' :: Qualified a -> [ModuleName]
  getQual' = maybe [] return . getQual

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
importToCoreFn (A.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: A.Declaration -> Maybe Ident
externToCoreFn (A.ExternDeclaration _ name _) = Just name
externToCoreFn _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, instances and
-- values are flattened into one list.
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (A.TypeRef _ _ Nothing) = []
exportToCoreFn (A.TypeOpRef _ _) = []
exportToCoreFn (A.ValueRef _ name) = [name]
exportToCoreFn (A.ValueOpRef _ _) = []
exportToCoreFn (A.TypeClassRef _ _) = []
exportToCoreFn (A.TypeInstanceRef _ name _) = [name]
exportToCoreFn (A.ModuleRef _ _) = []
exportToCoreFn (A.ReExportRef _ _ _) = []

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName
