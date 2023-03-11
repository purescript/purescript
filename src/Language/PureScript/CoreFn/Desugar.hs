module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Prelude
import Protolude (ordNub, orEmpty)

import Control.Arrow (second)

import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Language.PureScript.AST.Literals ( Literal(BooleanLiteral) )
import Language.PureScript.AST.SourcePos ( pattern NullSourceSpan, SourceSpan(spanName) )
import Language.PureScript.AST.Traversals ( everythingOnValues )
import Language.PureScript.Comments ( Comment )
import Language.PureScript.CoreFn.Ann ( ssAnn, Ann )
import Language.PureScript.CoreFn.Binders ( Binder(..) )
import Language.PureScript.CoreFn.Expr ( Bind(..), CaseAlternative(CaseAlternative), Expr(..), Guard )
import Language.PureScript.CoreFn.Meta ( ConstructorType(SumType, ProductType), Meta(..) )
import Language.PureScript.CoreFn.Module ( Module(Module) )
import Language.PureScript.Crash ( internalError )
import Language.PureScript.Environment ( isDictTypeName, lookupConstructor, lookupValue, DataDeclType(..), Environment(dataConstructors), NameKind(External) )
import Language.PureScript.Names ( pattern ByNullSourcePos, getQual, Ident(Ident), ModuleName, ProperName(runProperName), ProperNameType(TypeName, ConstructorName), Qualified(..), QualifiedBy(ByModuleName) )
import Language.PureScript.Types ( SourceType )
import Language.PureScript.AST.Binders qualified as ASTB ( Binder(..) )
import Language.PureScript.AST.Declarations qualified as ASTD ( pattern MkUnguarded, pattern TypeFixityDeclaration, pattern ValueDecl, pattern ValueFixityDeclaration, CaseAlternative(CaseAlternative), DataConstructorDeclaration(dataCtorName), Declaration(DataDeclaration, DataBindingGroupDeclaration, BindingGroupDeclaration, ExternDeclaration, TypeInstanceDeclaration, ImportDeclaration), DeclarationRef(..), ExportSource(exportSourceImportedFrom), Expr(Literal, ObjectUpdate, Abs, App, Accessor, Unused, IfThenElse, Case, TypedValue, Let, PositionedValue, Constructor, Var), Guard(ConditionGuard), GuardedExpr(..), Module(..), WhereProvenance(..) )
import Language.PureScript.Constants.Prim qualified as C

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: Environment -> ASTD.Module -> Module Ann
moduleToCoreFn _ (ASTD.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (ASTD.Module modSS coms mn decls (Just exps)) =
  let imports = mapMaybe importToCoreFn decls ++ fmap (ssAnn modSS,) (findQualModules decls)
      imports' = dedupeImports imports
      exps' = ordNub $ concatMap exportToCoreFn exps
      reExps = M.map ordNub $ M.unionsWith (++) (mapMaybe (fmap reExportsToCoreFn . toReExportRef) exps)
      externs = ordNub $ mapMaybe externToCoreFn decls
      decls' = concatMap declToCoreFn decls
  in Module modSS coms mn (spanName modSS) imports' exps' reExps externs decls'
  where
  -- | Creates a map from a module name to the re-export references defined in
  -- that module.
  reExportsToCoreFn :: (ModuleName, ASTD.DeclarationRef) -> M.Map ModuleName [Ident]
  reExportsToCoreFn (mn', ref') = M.singleton mn' (exportToCoreFn ref')

  toReExportRef :: ASTD.DeclarationRef -> Maybe (ModuleName, ASTD.DeclarationRef)
  toReExportRef (ASTD.ReExportRef _ src ref) =
      fmap
        (, ref)
        (ASTD.exportSourceImportedFrom src)
  toReExportRef _ = Nothing

  -- | Remove duplicate imports
  dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
  dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

  ssA :: SourceSpan -> Ann
  ssA ss = (ss, [], Nothing, Nothing)

  -- | Desugars member declarations from AST to CoreFn representation.
  declToCoreFn :: ASTD.Declaration -> [Bind Ann]
  declToCoreFn (ASTD.DataDeclaration (ss, com) Newtype _ _ [ctor]) =
    [NonRec (ss, [], Nothing, declMeta) (properToIdent $ ASTD.dataCtorName ctor) $
      Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var (ssAnn ss) $ Qualified ByNullSourcePos (Ident "x"))]
    where
    declMeta = isDictTypeName (ASTD.dataCtorName ctor) `orEmpty` IsTypeClassConstructor
  declToCoreFn d@(ASTD.DataDeclaration _ Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn (ASTD.DataDeclaration (ss, com) Data tyName _ ctors) =
    flip fmap ctors $ \ctorDecl ->
      let
        ctor = ASTD.dataCtorName ctorDecl
        (_, _, _, fields) = lookupConstructor env (Qualified (ByModuleName mn) ctor)
      in NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing, Nothing) tyName ctor fields
  declToCoreFn (ASTD.DataBindingGroupDeclaration ds) =
    concatMap declToCoreFn ds
  declToCoreFn (ASTD.ValueDecl (ss, com) name _ _ [ASTD.MkUnguarded e]) =
    [NonRec (ssA ss) name (exprToCoreFn ss com Nothing e)]
  declToCoreFn (ASTD.BindingGroupDeclaration ds) =
    [Rec . NEL.toList $ fmap (\(((ss, com), name), _, e) -> ((ssA ss, name), exprToCoreFn ss com Nothing e)) ds]
  declToCoreFn _ = []

  -- | Desugars expressions from AST to CoreFn representation.
  exprToCoreFn :: SourceSpan -> [Comment] -> Maybe SourceType -> ASTD.Expr -> Expr Ann
  exprToCoreFn _ com ty (ASTD.Literal ss lit) =
    Literal (ss, com, ty, Nothing) (fmap (exprToCoreFn ss com Nothing) lit)
  exprToCoreFn ss com ty (ASTD.Accessor name v) =
    Accessor (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (ASTD.ObjectUpdate obj vs) =
    ObjectUpdate (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing obj) $ fmap (second (exprToCoreFn ss [] Nothing)) vs
  exprToCoreFn ss com ty (ASTD.Abs (ASTB.VarBinder _ name) v) =
    Abs (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ _ _ (ASTD.Abs _ _) =
    internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ss com ty (ASTD.App v1 v2) =
    App (ss, com, ty, (isDictCtor v1 || isSynthetic v2) `orEmpty` IsSyntheticApp) v1' v2'
    where
    v1' = exprToCoreFn ss [] Nothing v1
    v2' = exprToCoreFn ss [] Nothing v2
    isDictCtor = \case
      ASTD.Constructor _ (Qualified _ name) -> isDictTypeName name
      _ -> False
    isSynthetic = \case
      ASTD.App v3 v4            -> isDictCtor v3 || isSynthetic v3 && isSynthetic v4
      ASTD.Accessor _ v3        -> isSynthetic v3
      ASTD.Var NullSourceSpan _ -> True
      ASTD.Unused{}             -> True
      _                      -> False
  exprToCoreFn ss com ty (ASTD.Unused _) =
    Var (ss, com, ty, Nothing) C.I_undefined
  exprToCoreFn _ com ty (ASTD.Var ss ident) =
    Var (ss, com, ty, getValueMeta ident) ident
  exprToCoreFn ss com ty (ASTD.IfThenElse v1 v2 v3) =
    Case (ss, com, ty, Nothing) [exprToCoreFn ss [] Nothing v1]
      [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                        (Right $ exprToCoreFn ss [] Nothing v2)
      , CaseAlternative [NullBinder (ssAnn ss)]
                        (Right $ exprToCoreFn ss [] Nothing v3) ]
  exprToCoreFn _ com ty (ASTD.Constructor ss name) =
    Var (ss, com, ty, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ss com ty (ASTD.Case vs alts) =
    Case (ss, com, ty, Nothing) (fmap (exprToCoreFn ss [] Nothing) vs) (fmap (altToCoreFn ss) alts)
  exprToCoreFn ss com _ (ASTD.TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v
  exprToCoreFn ss com ty (ASTD.Let w ds v) =
    Let (ss, com, ty, getLetMeta w) (concatMap declToCoreFn ds) (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ com ty (ASTD.PositionedValue ss com1 v) =
    exprToCoreFn ss (com ++ com1) ty v
  exprToCoreFn _ _ _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- | Desugars case alternatives from AST to CoreFn representation.
  altToCoreFn :: SourceSpan -> ASTD.CaseAlternative -> CaseAlternative Ann
  altToCoreFn ss (ASTD.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn ss []) bs) (go vs)
    where
    go :: [ASTD.GuardedExpr] -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go [ASTD.MkUnguarded e]
      = Right (exprToCoreFn ss [] Nothing e)
    go gs
      = Left [ (exprToCoreFn ss [] Nothing cond, exprToCoreFn ss [] Nothing e)
             | ASTD.GuardedExpr g e <- gs
             , let cond = guardToExpr g
             ]

    guardToExpr [ASTD.ConditionGuard cond] = cond
    guardToExpr _ = internalError "Guard not correctly desugared"

  -- | Desugars case binders from AST to CoreFn representation.
  binderToCoreFn :: SourceSpan -> [Comment] -> ASTB.Binder -> Binder Ann
  binderToCoreFn _ com (ASTB.LiteralBinder ss lit) =
    LiteralBinder (ss, com, Nothing, Nothing) (fmap (binderToCoreFn ss com) lit)
  binderToCoreFn ss com ASTB.NullBinder =
    NullBinder (ss, com, Nothing, Nothing)
  binderToCoreFn _ com (ASTB.VarBinder ss name) =
    VarBinder (ss, com, Nothing, Nothing) name
  binderToCoreFn _ com (ASTB.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, com, Nothing, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (fmap (binderToCoreFn ss []) bs)
  binderToCoreFn _ com (ASTB.NamedBinder ss name b) =
    NamedBinder (ss, com, Nothing, Nothing) name (binderToCoreFn ss [] b)
  binderToCoreFn _ com (ASTB.PositionedBinder ss com1 b) =
    binderToCoreFn ss (com ++ com1) b
  binderToCoreFn ss com (ASTB.TypedBinder _ b) =
    binderToCoreFn ss com b
  binderToCoreFn _ _ ASTB.OpBinder{} =
    internalError "OpBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ ASTB.BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ ASTB.ParensInBinder{} =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- | Gets metadata for let bindings.
  getLetMeta :: ASTD.WhereProvenance -> Maybe Meta
  getLetMeta ASTD.FromWhere = Just IsWhere
  getLetMeta ASTD.FromLet = Nothing

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
findQualModules :: [ASTD.Declaration] -> [ModuleName]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
  in f `concatMap` decls
  where
  fqDecls :: ASTD.Declaration -> [ModuleName]
  fqDecls (ASTD.TypeInstanceDeclaration _ _ _ _ _ _ q _ _) = getQual' q
  fqDecls (ASTD.ValueFixityDeclaration _ _ q _) = getQual' q
  fqDecls (ASTD.TypeFixityDeclaration _ _ q _) = getQual' q
  fqDecls _ = []

  fqValues :: ASTD.Expr -> [ModuleName]
  fqValues (ASTD.Var _ q) = getQual' q
  fqValues (ASTD.Constructor _ q) = getQual' q
  fqValues _ = []

  fqBinders :: ASTB.Binder -> [ModuleName]
  fqBinders (ASTB.ConstructorBinder _ q _) = getQual' q
  fqBinders _ = []

  getQual' :: Qualified a -> [ModuleName]
  getQual' = maybe [] return . getQual

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: ASTD.Declaration -> Maybe (Ann, ModuleName)
importToCoreFn (ASTD.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: ASTD.Declaration -> Maybe Ident
externToCoreFn (ASTD.ExternDeclaration _ name _) = Just name
externToCoreFn _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, instances and
-- values are flattened into one list.
exportToCoreFn :: ASTD.DeclarationRef -> [Ident]
exportToCoreFn (ASTD.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (ASTD.TypeRef _ _ Nothing) = []
exportToCoreFn (ASTD.TypeOpRef _ _) = []
exportToCoreFn (ASTD.ValueRef _ name) = [name]
exportToCoreFn (ASTD.ValueOpRef _ _) = []
exportToCoreFn (ASTD.TypeClassRef _ _) = []
exportToCoreFn (ASTD.TypeInstanceRef _ name _) = [name]
exportToCoreFn (ASTD.ModuleRef _ _) = []
exportToCoreFn (ASTD.ReExportRef _ _ _) = []

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName
