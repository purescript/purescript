-- |
-- This module implements the desugaring pass which creates newtypes for type class dictionaries
-- and value declarations for type class instances.
--
module Language.PureScript.Sugar.TypeClasses
  ( desugarTypeClasses
  , typeClassMemberName
  , superClassDictionaryNames
  ) where

import Prelude

import Control.Arrow (first, second)
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState(..), StateT, evalStateT, modify)
import Control.Monad.Supply.Class (MonadSupply)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (find, foldl', partition)
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.List.NonEmpty qualified as NEL
import Data.Set qualified as S
import Data.Text (Text)
import Data.Traversable (for)
import Language.PureScript.AST.Declarations.ChainId (mkChainId)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), FunctionalDependency(..), NameKind(..), TypeClassData(..), dictTypeName, function, kindArity, makeTypeClassData, primClasses, primCoerceClasses, primIntClasses, primRowClasses, primRowListClasses, primSymbolClasses, primTypeErrorClasses, tyRecord)
import Language.PureScript.Errors hiding (isExported, nonEmpty)
import Language.PureScript.Externs (ExternsDeclaration(..), ExternsFile(..))
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, Name(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, disqualify, freshIdent, mkDeriveInstanceName, qualify, runIdent, runProperName)
import Language.PureScript.PSString (mkString)
import Language.PureScript.Sugar.CaseDeclarations (desugarCases)
import Language.PureScript.TypeClassDictionaries (superclassName)
import Language.PureScript.Types

type MemberMap = M.Map (ModuleName, ProperName 'ClassName) TypeClassData

type Desugar = StateT MemberMap

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses
  :: (MonadSupply m, MonadError MultipleErrors m)
  => [ExternsFile]
  -> Module
  -> m Module
desugarTypeClasses externs = flip evalStateT initialState . desugarModule
  where
  initialState :: MemberMap
  initialState =
    mconcat
      [ M.mapKeys (qualify C.M_Prim) primClasses
      , M.mapKeys (qualify C.M_Prim_Coerce) primCoerceClasses
      , M.mapKeys (qualify C.M_Prim_Row) primRowClasses
      , M.mapKeys (qualify C.M_Prim_RowList) primRowListClasses
      , M.mapKeys (qualify C.M_Prim_Symbol) primSymbolClasses
      , M.mapKeys (qualify C.M_Prim_Int) primIntClasses
      , M.mapKeys (qualify C.M_Prim_TypeError) primTypeErrorClasses
      , M.fromList (externs >>= \ExternsFile{..} -> mapMaybe (fromExternsDecl efModuleName) efDeclarations)
      ]

  fromExternsDecl
    :: ModuleName
    -> ExternsDeclaration
    -> Maybe ((ModuleName, ProperName 'ClassName), TypeClassData)
  fromExternsDecl mn (EDClass name args members implies deps tcIsEmpty) = Just ((mn, name), typeClass) where
    typeClass = makeTypeClassData args members implies deps tcIsEmpty
  fromExternsDecl _ _ = Nothing

desugarModule
  :: (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> Desugar m Module
desugarModule (Module ss coms name decls (Just exps)) = do
  let (classDecls, restDecls) = partition isTypeClassDecl decls
      classVerts = fmap (\d -> (d, classDeclName d, superClassesNames d)) classDecls
  (classNewExpss, classDeclss) <- unzip <$> parU (stronglyConnComp classVerts) (desugarClassDecl name exps)
  (restNewExpss, restDeclss) <- unzip <$> parU restDecls (desugarDecl name exps)
  return $ Module ss coms name (concat restDeclss ++ concat classDeclss) $ Just (exps ++ catMaybes restNewExpss ++ catMaybes classNewExpss)
  where
  desugarClassDecl :: (MonadSupply m, MonadError MultipleErrors m)
    => ModuleName
    -> [DeclarationRef]
    -> SCC Declaration
    -> Desugar m (Maybe DeclarationRef, [Declaration])
  desugarClassDecl name' exps' (AcyclicSCC d) = desugarDecl name' exps' d
  desugarClassDecl _ _ (CyclicSCC ds')
    | Just ds'' <- nonEmpty ds' = throwError . errorMessage' (declSourceSpan (NEL.head ds'')) $ CycleInTypeClassDeclaration (NEL.map classDeclName ds'')
    | otherwise = internalError "desugarClassDecl: empty CyclicSCC"

  superClassesNames :: Declaration -> [Qualified (ProperName 'ClassName)]
  superClassesNames (TypeClassDeclaration _ _ _ implies _ _) = fmap constraintName implies
  superClassesNames _ = []

  constraintName :: SourceConstraint -> Qualified (ProperName 'ClassName)
  constraintName (Constraint _ cName _ _ _) = cName

  classDeclName :: Declaration -> Qualified (ProperName 'ClassName)
  classDeclName (TypeClassDeclaration _ pn _ _ _ _) = Qualified (ByModuleName name) pn
  classDeclName _ = internalError "Expected TypeClassDeclaration"

desugarModule _ = internalError "Exports should have been elaborated in name desugaring"

{- Desugar type class and type class instance declarations
--
-- Type classes become newtypes for their dictionaries, and type instances become dictionary declarations.
-- Additional values are generated to access individual members of a dictionary, with the appropriate type.
--
-- E.g. the following
--
--   module Test where
--
--   class Foo a where
--     foo :: a -> a
--
--   instance fooString :: Foo String where
--     foo s = s ++ s
--
--   instance fooArray :: (Foo a) => Foo [a] where
--     foo = map foo
--
--   {- Superclasses -}
--
--   class (Foo a) <= Sub a where
--     sub :: a
--
--   instance subString :: Sub String where
--     sub = ""
--
-- becomes:
--
--   <TypeClassDeclaration Foo ...>
--
--   newtype Foo$Dict a = Foo$Dict { foo :: a -> a }
--
--   -- this following type is marked as not needing to be checked so a new Abs
--   -- is not introduced around the definition in type checking, but when
--   -- called the dictionary value is still passed in for the `dict` argument
--   foo :: forall a. (Foo$Dict a) => a -> a
--   foo (Foo$Dict dict) = dict.foo
--
--   fooString :: Foo$Dict String
--   fooString = Foo$Dict { foo: \s -> s ++ s }
--
--   fooArray :: forall a. (Foo$Dict a) => Foo$Dict [a]
--   fooArray = Foo$Dict { foo: map foo }
--
--   {- Superclasses -}
--
--   <TypeClassDeclaration Sub ...>
--
--   newtype Sub$Dict a = Sub$Dict { sub :: a
--                                 , "Foo0" :: {} -> Foo$Dict a
--                                 }
--
--   -- As with `foo` above, this type is unchecked at the declaration
--   sub :: forall a. (Sub$Dict a) => a
--   sub (Sub$Dict dict) = dict.sub
--
--   subString :: Sub$Dict String
--   subString = Sub$Dict { sub: "",
--                        , "Foo0": \_ -> <DeferredDictionary Foo String>
--                        }
--
-- and finally as the generated javascript:
--
--   var foo = function (dict) {
--       return dict.foo;
--   };
--
--   var fooString = {
--      foo: function (s) {
--          return s + s;
--      }
--   };
--
--   var fooArray = function (dictFoo) {
--       return {
--           foo: map(foo(dictFoo))
--       };
--   };
--
--   var sub = function (dict) {
--       return dict.sub;
--   };
--
--   var subString = {
--       sub: "",
--       Foo0: function () {
--           return fooString;
--       }
--   };
-}
desugarDecl
  :: (MonadSupply m, MonadError MultipleErrors m)
  => ModuleName
  -> [DeclarationRef]
  -> Declaration
  -> Desugar m (Maybe DeclarationRef, [Declaration])
desugarDecl mn exps = go
  where
  go d@(TypeClassDeclaration sa name args implies deps members) = do
    modify (M.insert (mn, name) (makeTypeClassData args (map memberToNameAndType members) implies deps False))
    return (Nothing, d : typeClassDictionaryDeclaration sa name args implies members : map (typeClassMemberToDictionaryAccessor mn name args) members)
  go (TypeInstanceDeclaration sa na chainId idx name deps className tys body) = do
    name' <- desugarInstName name
    let d = TypeInstanceDeclaration sa na chainId idx (Right name') deps className tys body
    let explicitOrNot = case body of
          DerivedInstance -> Left $ DerivedInstancePlaceholder className KnownClassStrategy
          NewtypeInstance -> Left $ DerivedInstancePlaceholder className NewtypeStrategy
          ViaInstance viaTy -> Left $ DerivedInstancePlaceholder className (ViaStrategy viaTy)
          ExplicitInstance members -> Right members
    dictDecl <- case explicitOrNot of
      Right members
        | className == C.Coercible ->
          throwError . errorMessage' (fst sa) $ InvalidCoercibleInstanceDeclaration tys
        | otherwise -> do
          desugared <- desugarCases members
          typeInstanceDictionaryDeclaration sa name' mn deps className tys desugared
      Left dict ->
        let
          dictTy = foldl srcTypeApp (srcTypeConstructor (fmap (coerceProperName . dictTypeName) className)) tys
          constrainedTy = quantify (foldr srcConstrainedType dictTy deps)
        in
          return $ ValueDecl sa name' Private [] [MkUnguarded (TypedValue True dict constrainedTy)]
    return (expRef name' className tys, [d, dictDecl])
  go (DeriveClause sa _ddt tyName tyVars className extraArgs body) = do
    memberMap <- get
    let classMod = case className of
          Qualified (ByModuleName m) _ -> m
          _ -> mn
        classData = M.lookup (classMod, disqualify className) memberMap
        classArgs = maybe [] typeClassArguments classData
        classDeps = maybe [] typeClassDependencies classData
        ss = fst sa
    instArgs <- case computeInstArgs classArgs classDeps tyName tyVars extraArgs of
      Just args -> pure args
      Nothing -> throwError . errorMessage' ss $ DeriveClauseArityError className (length classArgs)
    let chainId = mkChainId (spanName ss) (spanStart ss)
        name' = Left $ mkDeriveInstanceName (disqualify className) (runProperName tyName)
    go (TypeInstanceDeclaration sa sa chainId 0 name' [] className instArgs body)

  go other = return (Nothing, [other])

  -- Compute the type arguments to pass to the class in the generated
  -- instance declaration. Uses the class param's kind to determine how
  -- many of the data type's variables to apply. For multi-param classes,
  -- params that are fully determined by fundeps get wildcards.
  --
  -- Examples for `data T a b`:
  --   derive (Eq)      → Eq (T a b)       (kind Type, drop 0 vars)
  --   derive (Functor) → Functor (T a)    (kind Type → Type, drop 1 var)
  --   derive (Generic) → Generic (T a b) _ (fundep a → rep, wildcard)
  computeInstArgs
    :: [(Text, Maybe SourceType)]
    -> [FunctionalDependency]
    -> ProperName 'TypeName
    -> [(Text, Maybe SourceType)]
    -> [SourceType]
    -> Maybe [SourceType]
  computeInstArgs _ _ _ _ extraArgs
    | not (null extraArgs) = Just extraArgs
  computeInstArgs classArgs classDeps tyName tyVars _
    | length classArgs == 1 =
        let kind = snd =<< listToMaybe classArgs
            dropCount = maybe 0 kindArity kind
            applyCount = max 0 (length tyVars - dropCount)
            tyCon = srcTypeConstructor (Qualified (ByModuleName mn) tyName)
            applied = foldl' srcTypeApp tyCon (map (srcTypeVar . fst) (take applyCount tyVars))
        in Just [applied]
    | determinedByFirst classArgs classDeps =
        let tyCon = srcTypeConstructor (Qualified (ByModuleName mn) tyName)
            applied = foldl' srcTypeApp tyCon (map (srcTypeVar . fst) tyVars)
        in Just [applied, srcTypeWildcard]
    | otherwise = Nothing

  -- Check if all class params after the first are determined by the first
  -- via functional dependencies (e.g. Generic a rep | a -> rep).
  determinedByFirst :: [(Text, Maybe SourceType)] -> [FunctionalDependency] -> Bool
  determinedByFirst args deps =
    length args > 1 && all (`elem` determined) [1 .. length args - 1]
    where
      determined = concatMap fdDetermined $ filter ((== [0]) . fdDeterminers) deps


  -- Completes the name generation for type class instances that do not have
  -- a unique name defined in source code.
  desugarInstName :: MonadSupply m => Either Text Ident -> Desugar m Ident
  desugarInstName = either freshIdent pure

  expRef :: Ident -> Qualified (ProperName 'ClassName) -> [SourceType] -> Maybe DeclarationRef
  expRef name className tys
    | isExportedClass className && all (all isExportedType . getConstructors) tys =
        Just $ TypeInstanceRef genSpan name UserNamed
    | otherwise = Nothing

  isExportedClass :: Qualified (ProperName 'ClassName) -> Bool
  isExportedClass = isExported (elem . TypeClassRef genSpan)

  isExportedType :: Qualified (ProperName 'TypeName) -> Bool
  isExportedType = isExported $ \pn -> isJust . find (matchesTypeRef pn)

  isExported
    :: (ProperName a -> [DeclarationRef] -> Bool)
    -> Qualified (ProperName a)
    -> Bool
  isExported test (Qualified (ByModuleName mn') pn) = mn /= mn' || test pn exps
  isExported _ _ = internalError "Names should have been qualified in name desugaring"

  matchesTypeRef :: ProperName 'TypeName -> DeclarationRef -> Bool
  matchesTypeRef pn (TypeRef _ pn' _) = pn == pn'
  matchesTypeRef _ _ = False

  getConstructors :: SourceType -> [Qualified (ProperName 'TypeName)]
  getConstructors = everythingOnTypes (++) getConstructor
    where
    getConstructor (TypeConstructor _ tcname) = [tcname]
    getConstructor _ = []

  genSpan :: SourceSpan
  genSpan = internalModuleSourceSpan "<generated>"

memberToNameAndType :: Declaration -> (Ident, SourceType)
memberToNameAndType (TypeDeclaration td) = unwrapTypeDeclaration td
memberToNameAndType _ = internalError "Invalid declaration in type class definition"

typeClassDictionaryDeclaration
  :: SourceAnn
  -> ProperName 'ClassName
  -> [(Text, Maybe SourceType)]
  -> [SourceConstraint]
  -> [Declaration]
  -> Declaration
typeClassDictionaryDeclaration sa name args implies members =
  let superclassTypes = superClassDictionaryNames implies `zip`
        [ function unit (foldl srcTypeApp (srcTypeConstructor (fmap (coerceProperName . dictTypeName) superclass)) tyArgs)
        | (Constraint _ superclass _ tyArgs _) <- implies
        ]
      members' = map (first runIdent . memberToNameAndType) members
      mtys = members' ++ superclassTypes
      toRowListItem (l, t) = srcRowListItem (Label $ mkString l) t
      ctor = DataConstructorDeclaration sa (coerceProperName $ dictTypeName name)
        [(Ident "dict", srcTypeApp tyRecord $ rowFromList (map toRowListItem mtys, srcREmpty))]
  in DataDeclaration sa Newtype (coerceProperName $ dictTypeName name) args [ctor]

typeClassMemberToDictionaryAccessor
  :: ModuleName
  -> ProperName 'ClassName
  -> [(Text, Maybe SourceType)]
  -> Declaration
  -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration (TypeDeclarationData sa@(ss, _) ident ty)) =
  let className = Qualified (ByModuleName mn) name
      dictIdent = Ident "dict"
      dictObjIdent = Ident "v"
      ctor = ConstructorBinder ss (coerceProperName . dictTypeName <$> className) [VarBinder ss dictObjIdent]
      acsr = Accessor (mkString $ runIdent ident) (Var ss (Qualified ByNullSourcePos dictObjIdent))
      visibility = second (const TypeVarVisible) <$> args
  in ValueDecl sa ident Private []
    [MkUnguarded (
     TypedValue False (Abs (VarBinder ss dictIdent) (Case [Var ss $ Qualified ByNullSourcePos dictIdent] [CaseAlternative [ctor] [MkUnguarded acsr]])) $
       addVisibility visibility (moveQuantifiersToFront NullSourceAnn (quantify (srcConstrainedType (srcConstraint className [] (map (srcTypeVar . fst) args) Nothing) ty)))
    )]
typeClassMemberToDictionaryAccessor _ _ _ _ = internalError "Invalid declaration in type class definition"

unit :: SourceType
unit = srcTypeApp tyRecord srcREmpty

typeInstanceDictionaryDeclaration
  :: forall m
   . MonadError MultipleErrors m
  => SourceAnn
  -> Ident
  -> ModuleName
  -> [SourceConstraint]
  -> Qualified (ProperName 'ClassName)
  -> [SourceType]
  -> [Declaration]
  -> Desugar m Declaration
typeInstanceDictionaryDeclaration sa@(ss, _) name mn deps className tys decls =
  rethrow (addHint (ErrorInInstance className tys)) $ do
  m <- get

  -- Lookup the type arguments and member types for the type class
  TypeClassData{..} <-
    maybe (throwError . errorMessage' ss . UnknownName $ fmap TyClassName className) return $
      M.lookup (qualify mn className) m

  -- Replace the type arguments with the appropriate types in the member types
  let memberTypes = map (second (replaceAllTypeVars (zip (map fst typeClassArguments) tys)) . tuple3To2) typeClassMembers

  let declaredMembers = S.fromList $ mapMaybe declIdent decls

  -- Instance declarations with a Fail constraint are unreachable code, so
  -- we allow them to be empty.
  let unreachable = any ((C.Fail ==) . constraintClass) deps && null decls

  unless unreachable $
    case filter (\(ident, _) -> not $ S.member ident declaredMembers) memberTypes of
      hd : tl -> throwError . errorMessage' ss $ MissingClassMember (hd NEL.:| tl)
      [] -> pure ()

  -- Create values for the type instance members
  members <- zip (map typeClassMemberName decls) <$> traverse (memberToValue memberTypes) decls

  -- Create the type of the dictionary
  -- The type is a record type, but depending on type instance dependencies, may be constrained.
  -- The dictionary itself is a record literal (unless unreachable, in which case it's undefined).
  superclassesDicts <- for typeClassSuperclasses $ \(Constraint _ superclass _ suTyArgs _) -> do
    let tyArgs = map (replaceAllTypeVars (zip (map fst typeClassArguments) tys)) suTyArgs
    pure $ Abs (VarBinder ss UnusedIdent) (DeferredDictionary superclass tyArgs)
  let superclasses = superClassDictionaryNames typeClassSuperclasses `zip` superclassesDicts

  let props = Literal ss $ ObjectLiteral $ map (first mkString) (members ++ superclasses)
      dictTy = foldl srcTypeApp (srcTypeConstructor (fmap (coerceProperName . dictTypeName) className)) tys
      constrainedTy = quantify (foldr srcConstrainedType dictTy deps)
      dict = App (Constructor ss (fmap (coerceProperName . dictTypeName) className)) props
      mkTV = if unreachable then TypedValue False (Var nullSourceSpan C.I_undefined) else TypedValue True dict
      result = ValueDecl sa name Private [] [MkUnguarded (mkTV constrainedTy)]
  return result

  where

  memberToValue :: [(Ident, SourceType)] -> Declaration -> Desugar m Expr
  memberToValue tys' (ValueDecl (ss', _) ident _ [] [MkUnguarded val]) = do
    _ <- maybe (throwError . errorMessage' ss' $ ExtraneousClassMember ident className) return $ lookup ident tys'
    return val
  memberToValue _ _ = internalError "Invalid declaration in type instance definition"

declIdent :: Declaration -> Maybe Ident
declIdent (ValueDeclaration vd) = Just (valdeclIdent vd)
declIdent (TypeDeclaration td) = Just (tydeclIdent td)
declIdent _ = Nothing

typeClassMemberName :: Declaration -> Text
typeClassMemberName = maybe (internalError "typeClassMemberName: Invalid declaration in type class definition") runIdent . declIdent

superClassDictionaryNames :: [Constraint a] -> [Text]
superClassDictionaryNames supers =
  [ superclassName pn index
  | (index, Constraint _ pn _ _ _) <- zip [0..] supers
  ]

tuple3To2 :: (a, b, c) -> (a, b)
tuple3To2 (a, b, _) = (a, b)
