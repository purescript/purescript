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

import           Control.Arrow (first, second)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State
import           Control.Monad.Supply.Class
import           Data.Graph
import           Data.List (find, partition)
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, mapMaybe, isJust)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Traversable (for)
import qualified Language.PureScript.Constants.Prim as C
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors hiding (isExported, nonEmpty)
import           Language.PureScript.Externs
import           Language.PureScript.Label (Label(..))
import           Language.PureScript.Names
import           Language.PureScript.PSString (mkString)
import           Language.PureScript.Sugar.CaseDeclarations
import           Language.PureScript.TypeClassDictionaries (superclassName)
import           Language.PureScript.Types

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
      [ M.mapKeys (qualify C.Prim) primClasses
      , M.mapKeys (qualify C.PrimCoerce) primCoerceClasses
      , M.mapKeys (qualify C.PrimRow) primRowClasses
      , M.mapKeys (qualify C.PrimRowList) primRowListClasses
      , M.mapKeys (qualify C.PrimSymbol) primSymbolClasses
      , M.mapKeys (qualify C.PrimInt) primIntClasses
      , M.mapKeys (qualify C.PrimTypeError) primTypeErrorClasses
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
  go other = return (Nothing, [other])

  -- |
  -- Completes the name generation for type class instances that do not have
  -- a unique name defined in source code.
  desugarInstName :: MonadSupply m => Either Text Ident -> Desugar m Ident
  desugarInstName = either freshIdent pure

  expRef :: Ident -> Qualified (ProperName 'ClassName) -> [SourceType] -> Maybe DeclarationRef
  expRef name className tys
    | isExportedClass className && all isExportedType (getConstructors `concatMap` tys) = Just $ TypeInstanceRef genSpan name UserNamed
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
  in ValueDecl sa ident Private []
    [MkUnguarded (
     TypedValue False (Abs (VarBinder ss dictIdent) (Case [Var ss $ Qualified ByNullSourcePos dictIdent] [CaseAlternative [ctor] [MkUnguarded acsr]])) $
       moveQuantifiersToFront (quantify (srcConstrainedType (srcConstraint className [] (map (srcTypeVar . fst) args) Nothing) ty))
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
  let memberTypes = map (second (replaceAllTypeVars (zip (map fst typeClassArguments) tys))) typeClassMembers

  let declaredMembers = S.fromList $ mapMaybe declIdent decls

  case filter (\(ident, _) -> not $ S.member ident declaredMembers) memberTypes of
    hd : tl -> throwError . errorMessage' ss $ MissingClassMember (hd NEL.:| tl)
    [] -> do
      -- Create values for the type instance members
      members <- zip (map typeClassMemberName decls) <$> traverse (memberToValue memberTypes) decls

      -- Create the type of the dictionary
      -- The type is a record type, but depending on type instance dependencies, may be constrained.
      -- The dictionary itself is a record literal.
      superclassesDicts <- for typeClassSuperclasses $ \(Constraint _ superclass _ suTyArgs _) -> do
        let tyArgs = map (replaceAllTypeVars (zip (map fst typeClassArguments) tys)) suTyArgs
        pure $ Abs (VarBinder ss UnusedIdent) (DeferredDictionary superclass tyArgs)
      let superclasses = superClassDictionaryNames typeClassSuperclasses `zip` superclassesDicts

      let props = Literal ss $ ObjectLiteral $ map (first mkString) (members ++ superclasses)
          dictTy = foldl srcTypeApp (srcTypeConstructor (fmap (coerceProperName . dictTypeName) className)) tys
          constrainedTy = quantify (foldr srcConstrainedType dictTy deps)
          dict = App (Constructor ss (fmap (coerceProperName . dictTypeName) className)) props
          result = ValueDecl sa name Private [] [MkUnguarded (TypedValue True dict constrainedTy)]
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
