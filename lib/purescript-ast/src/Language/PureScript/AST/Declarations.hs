{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for modules and declarations
--
module Language.PureScript.AST.Declarations where

import Prelude.Compat

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Functor.Identity

import Data.Aeson.TH
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.List.NonEmpty as NEL
import GHC.Generics (Generic)

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Literals
import Language.PureScript.AST.Operators
import Language.PureScript.AST.SourcePos
import Language.PureScript.Types
import Language.PureScript.PSString (PSString)
import Language.PureScript.Label (Label)
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Comments
import Language.PureScript.Environment
import qualified Language.PureScript.Constants.Prim as C

-- | A map of locally-bound names in scope.
type Context = [(Ident, SourceType)]

-- | Holds the data necessary to do type directed search for typed holes
data TypeSearch
  = TSBefore Environment
  -- ^ An Environment captured for later consumption by type directed search
  | TSAfter
    { tsAfterIdentifiers :: [(Qualified Text, SourceType)]
    -- ^ The identifiers that fully satisfy the subsumption check
    , tsAfterRecordFields :: Maybe [(Label, SourceType)]
    -- ^ Record fields that are available on the first argument to the typed
    -- hole
    }
  -- ^ Results of applying type directed search to the previously captured
  -- Environment
  deriving Show

onTypeSearchTypes :: (SourceType -> SourceType) -> TypeSearch -> TypeSearch
onTypeSearchTypes f = runIdentity . onTypeSearchTypesM (Identity . f)

onTypeSearchTypesM :: (Applicative m) => (SourceType -> m SourceType) -> TypeSearch -> m TypeSearch
onTypeSearchTypesM f (TSAfter i r) = TSAfter <$> traverse (traverse f) i <*> traverse (traverse (traverse f)) r
onTypeSearchTypesM _ (TSBefore env) = pure (TSBefore env)

-- | Error message hints, providing more detailed information about failure.
data ErrorMessageHint
  = ErrorUnifyingTypes SourceType SourceType
  | ErrorInExpression Expr
  | ErrorInModule ModuleName
  | ErrorInInstance (Qualified (ProperName 'ClassName)) [SourceType]
  | ErrorInSubsumption SourceType SourceType
  | ErrorCheckingAccessor Expr PSString
  | ErrorCheckingType Expr SourceType
  | ErrorCheckingKind SourceType SourceType
  | ErrorCheckingGuard
  | ErrorInferringType Expr
  | ErrorInferringKind SourceType
  | ErrorInApplication Expr SourceType Expr
  | ErrorInDataConstructor (ProperName 'ConstructorName)
  | ErrorInTypeConstructor (ProperName 'TypeName)
  | ErrorInBindingGroup (NEL.NonEmpty Ident)
  | ErrorInDataBindingGroup [ProperName 'TypeName]
  | ErrorInTypeSynonym (ProperName 'TypeName)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | ErrorInTypeClassDeclaration (ProperName 'ClassName)
  | ErrorInKindDeclaration (ProperName 'TypeName)
  | ErrorInRoleDeclaration (ProperName 'TypeName)
  | ErrorInForeignImport Ident
  | ErrorSolvingConstraint SourceConstraint
  | MissingConstructorImportForCoercible (Qualified (ProperName 'ConstructorName))
  | PositionedError (NEL.NonEmpty SourceSpan)
  deriving (Show)

-- | Categories of hints
data HintCategory
  = ExprHint
  | KindHint
  | CheckHint
  | PositionHint
  | SolverHint
  | OtherHint
  deriving (Show, Eq)

-- |
-- A module declaration, consisting of comments about the module, a module name,
-- a list of declarations, and a list of the declarations that are
-- explicitly exported. If the export list is Nothing, everything is exported.
--
data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])
  deriving (Show)

-- | Return a module's name.
getModuleName :: Module -> ModuleName
getModuleName (Module _ _ name _ _) = name

-- | Return a module's source span.
getModuleSourceSpan :: Module -> SourceSpan
getModuleSourceSpan (Module ss _ _ _ _) = ss

-- | Return a module's declarations.
getModuleDeclarations :: Module -> [Declaration]
getModuleDeclarations (Module _ _ _ declarations _) = declarations

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
-- Will not import an unqualified module if that module has already been imported qualified.
-- (See #2197)
--
addDefaultImport :: Qualified ModuleName -> Module -> Module
addDefaultImport (Qualified toImportAs toImport) m@(Module ss coms mn decls exps) =
  if isExistingImport `any` decls || mn == toImport then m
  else Module ss coms mn (ImportDeclaration (ss, []) toImport Implicit toImportAs : decls) exps
  where
  isExistingImport (ImportDeclaration _ mn' _ as')
    | mn' == toImport =
        case toImportAs of
          Nothing -> True
          _ -> as' == toImportAs
  isExistingImport _ = False

-- | Adds import declarations to a module for an implicit Prim import and Prim
-- | qualified as Prim, as necessary.
importPrim :: Module -> Module
importPrim =
  let
    primModName = C.Prim
  in
    addDefaultImport (Qualified (Just primModName) primModName)
      . addDefaultImport (Qualified Nothing primModName)

-- |
-- An item in a list of explicit imports or exports
--
data DeclarationRef
  -- |
  -- A type class
  --
  = TypeClassRef SourceSpan (ProperName 'ClassName)
  -- |
  -- A type operator
  --
  | TypeOpRef SourceSpan (OpName 'TypeOpName)
  -- |
  -- A type constructor with data constructors
  --
  | TypeRef SourceSpan (ProperName 'TypeName) (Maybe [ProperName 'ConstructorName])
  -- |
  -- A value
  --
  | ValueRef SourceSpan Ident
  -- |
  -- A value-level operator
  --
  | ValueOpRef SourceSpan (OpName 'ValueOpName)
  -- |
  -- A type class instance, created during typeclass desugaring (name, class name, instance types)
  --
  | TypeInstanceRef SourceSpan Ident
  -- |
  -- A module, in its entirety
  --
  | ModuleRef SourceSpan ModuleName
  -- |
  -- A value re-exported from another module. These will be inserted during
  -- elaboration in name desugaring.
  --
  | ReExportRef SourceSpan ExportSource DeclarationRef
  deriving (Show, Generic, NFData, Serialise)

instance Eq DeclarationRef where
  (TypeClassRef _ name) == (TypeClassRef _ name') = name == name'
  (TypeOpRef _ name) == (TypeOpRef _ name') = name == name'
  (TypeRef _ name dctors) == (TypeRef _ name' dctors') = name == name' && dctors == dctors'
  (ValueRef _ name) == (ValueRef _ name') = name == name'
  (ValueOpRef _ name) == (ValueOpRef _ name') = name == name'
  (TypeInstanceRef _ name) == (TypeInstanceRef _ name') = name == name'
  (ModuleRef _ name) == (ModuleRef _ name') = name == name'
  (ReExportRef _ mn ref) == (ReExportRef _ mn' ref') = mn == mn' && ref == ref'
  _ == _ = False

instance Ord DeclarationRef where
  TypeClassRef _ name `compare` TypeClassRef _ name' = compare name name'
  TypeOpRef _ name `compare` TypeOpRef _ name' = compare name name'
  TypeRef _ name dctors `compare` TypeRef _ name' dctors' = compare name name' <> compare dctors dctors'
  ValueRef _ name `compare` ValueRef _ name' = compare name name'
  ValueOpRef _ name `compare` ValueOpRef _ name' = compare name name'
  TypeInstanceRef _ name `compare` TypeInstanceRef _ name' = compare name name'
  ModuleRef _ name `compare` ModuleRef _ name' = compare name name'
  ReExportRef _ mn ref `compare` ReExportRef _ mn' ref' = compare mn mn' <> compare ref ref'
  compare ref ref' =
    compare (orderOf ref) (orderOf ref')
      where
        orderOf :: DeclarationRef -> Int
        orderOf TypeClassRef{} = 0
        orderOf TypeOpRef{} = 1
        orderOf TypeRef{} = 2
        orderOf ValueRef{} = 3
        orderOf ValueOpRef{} = 4
        orderOf TypeInstanceRef{} = 5
        orderOf ModuleRef{} = 6
        orderOf ReExportRef{} = 7

data ExportSource =
  ExportSource
  { exportSourceImportedFrom :: Maybe ModuleName
  , exportSourceDefinedIn :: ModuleName
  }
  deriving (Eq, Ord, Show, Generic, NFData, Serialise)

declRefSourceSpan :: DeclarationRef -> SourceSpan
declRefSourceSpan (TypeRef ss _ _) = ss
declRefSourceSpan (TypeOpRef ss _) = ss
declRefSourceSpan (ValueRef ss _) = ss
declRefSourceSpan (ValueOpRef ss _) = ss
declRefSourceSpan (TypeClassRef ss _) = ss
declRefSourceSpan (TypeInstanceRef ss _) = ss
declRefSourceSpan (ModuleRef ss _) = ss
declRefSourceSpan (ReExportRef ss _ _) = ss

declRefName :: DeclarationRef -> Name
declRefName (TypeRef _ n _) = TyName n
declRefName (TypeOpRef _ n) = TyOpName n
declRefName (ValueRef _ n) = IdentName n
declRefName (ValueOpRef _ n) = ValOpName n
declRefName (TypeClassRef _ n) = TyClassName n
declRefName (TypeInstanceRef _ n) = IdentName n
declRefName (ModuleRef _ n) = ModName n
declRefName (ReExportRef _ _ ref) = declRefName ref

getTypeRef :: DeclarationRef -> Maybe (ProperName 'TypeName, Maybe [ProperName 'ConstructorName])
getTypeRef (TypeRef _ name dctors) = Just (name, dctors)
getTypeRef _ = Nothing

getTypeOpRef :: DeclarationRef -> Maybe (OpName 'TypeOpName)
getTypeOpRef (TypeOpRef _ op) = Just op
getTypeOpRef _ = Nothing

getValueRef :: DeclarationRef -> Maybe Ident
getValueRef (ValueRef _ name) = Just name
getValueRef _ = Nothing

getValueOpRef :: DeclarationRef -> Maybe (OpName 'ValueOpName)
getValueOpRef (ValueOpRef _ op) = Just op
getValueOpRef _ = Nothing

getTypeClassRef :: DeclarationRef -> Maybe (ProperName 'ClassName)
getTypeClassRef (TypeClassRef _ name) = Just name
getTypeClassRef _ = Nothing

isModuleRef :: DeclarationRef -> Bool
isModuleRef ModuleRef{} = True
isModuleRef _ = False

-- |
-- The data type which specifies type of import declaration
--
data ImportDeclarationType
  -- |
  -- An import with no explicit list: `import M`.
  --
  = Implicit
  -- |
  -- An import with an explicit list of references to import: `import M (foo)`
  --
  | Explicit [DeclarationRef]
  -- |
  -- An import with a list of references to hide: `import M hiding (foo)`
  --
  | Hiding [DeclarationRef]
  deriving (Eq, Show, Generic, Serialise)

isImplicit :: ImportDeclarationType -> Bool
isImplicit Implicit = True
isImplicit _ = False

isExplicit :: ImportDeclarationType -> Bool
isExplicit (Explicit _) = True
isExplicit _ = False

-- | A role declaration assigns a list of roles to a type constructor's
-- parameters, e.g.:
--
-- @type role T representational phantom@
--
-- In this example, @T@ is the identifier and @[representational, phantom]@ is
-- the list of roles (@T@ presumably having two parameters).
data RoleDeclarationData = RoleDeclarationData
  { rdeclSourceAnn :: !SourceAnn
  , rdeclIdent :: !(ProperName 'TypeName)
  , rdeclRoles :: ![Role]
  } deriving (Show, Eq)

-- | A type declaration assigns a type to an identifier, eg:
--
-- @identity :: forall a. a -> a@
--
-- In this example @identity@ is the identifier and @forall a. a -> a@ the type.
data TypeDeclarationData = TypeDeclarationData
  { tydeclSourceAnn :: !SourceAnn
  , tydeclIdent :: !Ident
  , tydeclType :: !SourceType
  } deriving (Show, Eq)

overTypeDeclaration :: (TypeDeclarationData -> TypeDeclarationData) -> Declaration -> Declaration
overTypeDeclaration f d = maybe d (TypeDeclaration . f) (getTypeDeclaration d)

getTypeDeclaration :: Declaration -> Maybe TypeDeclarationData
getTypeDeclaration (TypeDeclaration d) = Just d
getTypeDeclaration _ = Nothing

unwrapTypeDeclaration :: TypeDeclarationData -> (Ident, SourceType)
unwrapTypeDeclaration td = (tydeclIdent td, tydeclType td)

-- | A value declaration assigns a name and potential binders, to an expression (or multiple guarded expressions).
--
-- @double x = x + x@
--
-- In this example @double@ is the identifier, @x@ is a binder and @x + x@ is the expression.
data ValueDeclarationData a = ValueDeclarationData
  { valdeclSourceAnn :: !SourceAnn
  , valdeclIdent :: !Ident
  -- ^ The declared value's name
  , valdeclName :: !NameKind
  -- ^ Whether or not this value is exported/visible
  , valdeclBinders :: ![Binder]
  , valdeclExpression :: !a
  } deriving (Show, Functor, Foldable, Traversable)

overValueDeclaration :: (ValueDeclarationData [GuardedExpr] -> ValueDeclarationData [GuardedExpr]) -> Declaration -> Declaration
overValueDeclaration f d = maybe d (ValueDeclaration . f) (getValueDeclaration d)

getValueDeclaration :: Declaration -> Maybe (ValueDeclarationData [GuardedExpr])
getValueDeclaration (ValueDeclaration d) = Just d
getValueDeclaration _ = Nothing

pattern ValueDecl :: SourceAnn -> Ident -> NameKind -> [Binder] -> [GuardedExpr] -> Declaration
pattern ValueDecl sann ident name binders expr
  = ValueDeclaration (ValueDeclarationData sann ident name binders expr)

data DataConstructorDeclaration = DataConstructorDeclaration
  { dataCtorAnn :: !SourceAnn
  , dataCtorName :: !(ProperName 'ConstructorName)
  , dataCtorFields :: ![(Ident, SourceType)]
  } deriving (Show, Eq)

mapDataCtorFields :: ([(Ident, SourceType)] -> [(Ident, SourceType)]) -> DataConstructorDeclaration -> DataConstructorDeclaration
mapDataCtorFields f DataConstructorDeclaration{..} = DataConstructorDeclaration { dataCtorFields = f dataCtorFields, .. }

traverseDataCtorFields :: Monad m => ([(Ident, SourceType)] -> m [(Ident, SourceType)]) -> DataConstructorDeclaration -> m DataConstructorDeclaration
traverseDataCtorFields f DataConstructorDeclaration{..} = DataConstructorDeclaration dataCtorAnn dataCtorName <$> f dataCtorFields

-- |
-- The data type of declarations
--
data Declaration
  -- |
  -- A data type declaration (data or newtype, name, arguments, data constructors)
  --
  = DataDeclaration SourceAnn DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [DataConstructorDeclaration]
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
  | DataBindingGroupDeclaration (NEL.NonEmpty Declaration)
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
  | TypeSynonymDeclaration SourceAnn (ProperName 'TypeName) [(Text, Maybe SourceType)] SourceType
  -- |
  -- A kind signature declaration
  --
  | KindDeclaration SourceAnn KindSignatureFor (ProperName 'TypeName) SourceType
  -- |
  -- A role declaration (name, roles)
  --
  | RoleDeclaration {-# UNPACK #-} !RoleDeclarationData
  -- |
  -- A type declaration for a value (name, ty)
  --
  | TypeDeclaration {-# UNPACK #-} !TypeDeclarationData
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
  | ValueDeclaration {-# UNPACK #-} !(ValueDeclarationData [GuardedExpr])
  -- |
  -- A declaration paired with pattern matching in let-in expression (binder, optional guard, value)
  | BoundValueDeclaration SourceAnn Binder Expr
  -- |
  -- A minimal mutually recursive set of value declarations
  --
  | BindingGroupDeclaration (NEL.NonEmpty ((SourceAnn, Ident), NameKind, Expr))
  -- |
  -- A foreign import declaration (name, type)
  --
  | ExternDeclaration SourceAnn Ident SourceType
  -- |
  -- A data type foreign import (name, kind)
  --
  | ExternDataDeclaration SourceAnn (ProperName 'TypeName) SourceType
  -- |
  -- A fixity declaration
  --
  | FixityDeclaration SourceAnn (Either ValueFixity TypeFixity)
  -- |
  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
  --
  | ImportDeclaration SourceAnn ModuleName ImportDeclarationType (Maybe ModuleName)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
  | TypeClassDeclaration SourceAnn (ProperName 'ClassName) [(Text, Maybe SourceType)] [SourceConstraint] [FunctionalDependency] [Declaration]
  -- |
  -- A type instance declaration (instance chain, chain index, name,
  -- dependencies, class name, instance types, member declarations)
  --
  | TypeInstanceDeclaration SourceAnn [Ident] Integer Ident [SourceConstraint] (Qualified (ProperName 'ClassName)) [SourceType] TypeInstanceBody
  deriving (Show)

data ValueFixity = ValueFixity Fixity (Qualified (Either Ident (ProperName 'ConstructorName))) (OpName 'ValueOpName)
  deriving (Eq, Ord, Show)

data TypeFixity = TypeFixity Fixity (Qualified (ProperName 'TypeName)) (OpName 'TypeOpName)
  deriving (Eq, Ord, Show)

pattern ValueFixityDeclaration :: SourceAnn -> Fixity -> Qualified (Either Ident (ProperName 'ConstructorName)) -> OpName 'ValueOpName -> Declaration
pattern ValueFixityDeclaration sa fixity name op = FixityDeclaration sa (Left (ValueFixity fixity name op))

pattern TypeFixityDeclaration :: SourceAnn -> Fixity -> Qualified (ProperName 'TypeName) -> OpName 'TypeOpName -> Declaration
pattern TypeFixityDeclaration sa fixity name op = FixityDeclaration sa (Right (TypeFixity fixity name op))

-- | The members of a type class instance declaration
data TypeInstanceBody
  = DerivedInstance
  -- ^ This is a derived instance
  | NewtypeInstance
  -- ^ This is an instance derived from a newtype
  | NewtypeInstanceWithDictionary Expr
  -- ^ This is an instance derived from a newtype, desugared to include a
  -- dictionary for the type under the newtype.
  | ExplicitInstance [Declaration]
  -- ^ This is a regular (explicit) instance
  deriving (Show)

mapTypeInstanceBody :: ([Declaration] -> [Declaration]) -> TypeInstanceBody -> TypeInstanceBody
mapTypeInstanceBody f = runIdentity . traverseTypeInstanceBody (Identity . f)

-- | A traversal for TypeInstanceBody
traverseTypeInstanceBody :: (Applicative f) => ([Declaration] -> f [Declaration]) -> TypeInstanceBody -> f TypeInstanceBody
traverseTypeInstanceBody f (ExplicitInstance ds) = ExplicitInstance <$> f ds
traverseTypeInstanceBody _ other = pure other

-- | What sort of declaration the kind signature applies to.
data KindSignatureFor
  = DataSig
  | NewtypeSig
  | TypeSynonymSig
  | ClassSig
  deriving (Eq, Ord, Show)

declSourceAnn :: Declaration -> SourceAnn
declSourceAnn (DataDeclaration sa _ _ _ _) = sa
declSourceAnn (DataBindingGroupDeclaration ds) = declSourceAnn (NEL.head ds)
declSourceAnn (TypeSynonymDeclaration sa _ _ _) = sa
declSourceAnn (KindDeclaration sa _ _ _) = sa
declSourceAnn (RoleDeclaration rd) = rdeclSourceAnn rd
declSourceAnn (TypeDeclaration td) = tydeclSourceAnn td
declSourceAnn (ValueDeclaration vd) = valdeclSourceAnn vd
declSourceAnn (BoundValueDeclaration sa _ _) = sa
declSourceAnn (BindingGroupDeclaration ds) = let ((sa, _), _, _) = NEL.head ds in sa
declSourceAnn (ExternDeclaration sa _ _) = sa
declSourceAnn (ExternDataDeclaration sa _ _) = sa
declSourceAnn (FixityDeclaration sa _) = sa
declSourceAnn (ImportDeclaration sa _ _ _) = sa
declSourceAnn (TypeClassDeclaration sa _ _ _ _ _) = sa
declSourceAnn (TypeInstanceDeclaration sa _ _ _ _ _ _ _) = sa

declSourceSpan :: Declaration -> SourceSpan
declSourceSpan = fst . declSourceAnn

declName :: Declaration -> Maybe Name
declName (DataDeclaration _ _ n _ _) = Just (TyName n)
declName (TypeSynonymDeclaration _ n _ _) = Just (TyName n)
declName (ValueDeclaration vd) = Just (IdentName (valdeclIdent vd))
declName (ExternDeclaration _ n _) = Just (IdentName n)
declName (ExternDataDeclaration _ n _) = Just (TyName n)
declName (FixityDeclaration _ (Left (ValueFixity _ _ n))) = Just (ValOpName n)
declName (FixityDeclaration _ (Right (TypeFixity _ _ n))) = Just (TyOpName n)
declName (TypeClassDeclaration _ n _ _ _ _) = Just (TyClassName n)
declName (TypeInstanceDeclaration _ _ _ n _ _ _ _) = Just (IdentName n)
declName ImportDeclaration{} = Nothing
declName BindingGroupDeclaration{} = Nothing
declName DataBindingGroupDeclaration{} = Nothing
declName BoundValueDeclaration{} = Nothing
declName KindDeclaration{} = Nothing
declName TypeDeclaration{} = Nothing
declName RoleDeclaration{} = Nothing

-- |
-- Test if a declaration is a value declaration
--
isValueDecl :: Declaration -> Bool
isValueDecl ValueDeclaration{} = True
isValueDecl _ = False

-- |
-- Test if a declaration is a data type declaration
--
isDataDecl :: Declaration -> Bool
isDataDecl DataDeclaration{} = True
isDataDecl _ = False

-- |
-- Test if a declaration is a type synonym declaration
--
isTypeSynonymDecl :: Declaration -> Bool
isTypeSynonymDecl TypeSynonymDeclaration{} = True
isTypeSynonymDecl _ = False

-- |
-- Test if a declaration is a module import
--
isImportDecl :: Declaration -> Bool
isImportDecl ImportDeclaration{} = True
isImportDecl _ = False

-- |
-- Test if a declaration is a role declaration
--
isRoleDecl :: Declaration -> Bool
isRoleDecl RoleDeclaration{} = True
isRoleDecl _ = False

-- |
-- Test if a declaration is a data type foreign import
--
isExternDataDecl :: Declaration -> Bool
isExternDataDecl ExternDataDeclaration{} = True
isExternDataDecl _ = False

-- |
-- Test if a declaration is a fixity declaration
--
isFixityDecl :: Declaration -> Bool
isFixityDecl FixityDeclaration{} = True
isFixityDecl _ = False

getFixityDecl :: Declaration -> Maybe (Either ValueFixity TypeFixity)
getFixityDecl (FixityDeclaration _ fixity) = Just fixity
getFixityDecl _ = Nothing

-- |
-- Test if a declaration is a foreign import
--
isExternDecl :: Declaration -> Bool
isExternDecl ExternDeclaration{} = True
isExternDecl _ = False

-- |
-- Test if a declaration is a type class instance declaration
--
isTypeClassInstanceDecl :: Declaration -> Bool
isTypeClassInstanceDecl TypeInstanceDeclaration{} = True
isTypeClassInstanceDecl _ = False

-- |
-- Test if a declaration is a type class declaration
--
isTypeClassDecl :: Declaration -> Bool
isTypeClassDecl TypeClassDeclaration{} = True
isTypeClassDecl _ = False

-- |
-- Test if a declaration is a kind signature declaration.
--
isKindDecl :: Declaration -> Bool
isKindDecl KindDeclaration{} = True
isKindDecl _ = False

-- |
-- Recursively flatten data binding groups in the list of declarations
flattenDecls :: [Declaration] -> [Declaration]
flattenDecls = concatMap flattenOne
    where flattenOne :: Declaration -> [Declaration]
          flattenOne (DataBindingGroupDeclaration decls) = concatMap flattenOne decls
          flattenOne d = [d]

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
data Guard = ConditionGuard Expr
           | PatternGuard Binder Expr
           deriving (Show)

-- |
-- The right hand side of a binder in value declarations
-- and case expressions.
data GuardedExpr = GuardedExpr [Guard] Expr
                 deriving (Show)

pattern MkUnguarded :: Expr -> GuardedExpr
pattern MkUnguarded e = GuardedExpr [] e

-- |
-- Data type for expressions and terms
--
data Expr
  -- |
  -- A literal value
  --
  = Literal SourceSpan (Literal Expr)
  -- |
  -- A prefix -, will be desugared
  --
  | UnaryMinus SourceSpan Expr
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | BinaryNoParens Expr Expr Expr
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful, since it prevents
  -- certain traversals from matching.
  --
  | Parens Expr
  -- |
  -- An record property accessor expression (e.g. `obj.x` or `_.x`).
  -- Anonymous arguments will be removed during desugaring and expanded
  -- into a lambda that reads a property from a record.
  --
  | Accessor PSString Expr
  -- |
  -- Partial record update
  --
  | ObjectUpdate Expr [(PSString, Expr)]
  -- |
  -- Object updates with nested support: `x { foo { bar = e } }`
  -- Replaced during desugaring into a `Let` and nested `ObjectUpdate`s
  --
  | ObjectUpdateNested Expr (PathTree Expr)
  -- |
  -- Function introduction
  --
  | Abs Binder Expr
  -- |
  -- Function application
  --
  | App Expr Expr
  -- |
  -- Hint that an expression is unused.
  -- This is used to ignore type class dictionaries that are necessarily empty.
  -- The inner expression lets us solve subgoals before eliminating the whole expression.
  -- The code gen will render this as `undefined`, regardless of what the inner expression is.
  | Unused Expr
  -- |
  -- Variable
  --
  | Var SourceSpan (Qualified Ident)
  -- |
  -- An operator. This will be desugared into a function during the "operators"
  -- phase of desugaring.
  --
  | Op SourceSpan (Qualified (OpName 'ValueOpName))
  -- |
  -- Conditional (if-then-else expression)
  --
  | IfThenElse Expr Expr Expr
  -- |
  -- A data constructor
  --
  | Constructor SourceSpan (Qualified (ProperName 'ConstructorName))
  -- |
  -- A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  --
  | Case [Expr] [CaseAlternative]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Bool Expr SourceType
  -- |
  -- A let binding
  --
  | Let WhereProvenance [Declaration] Expr
  -- |
  -- A do-notation block
  --
  | Do (Maybe ModuleName) [DoNotationElement]
  -- |
  -- An ado-notation block
  --
  | Ado (Maybe ModuleName) [DoNotationElement] Expr
  -- |
  -- An application of a typeclass dictionary constructor. The value should be
  -- an ObjectLiteral.
  --
  | TypeClassDictionaryConstructorApp (Qualified (ProperName 'ClassName)) Expr
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary SourceConstraint
                        (M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))))
                        [ErrorMessageHint]
  -- |
  -- A typeclass dictionary accessor, the implementation is left unspecified until CoreFn desugaring.
  --
  | TypeClassDictionaryAccessor (Qualified (ProperName 'ClassName)) Ident
  -- |
  -- A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  --
  | DeferredDictionary (Qualified (ProperName 'ClassName)) [SourceType]
  -- |
  -- A placeholder for an anonymous function argument
  --
  | AnonymousArgument
  -- |
  -- A typed hole that will be turned into a hint/error during typechecking
  --
  | Hole Text
  -- |
  -- A value with source position information
  --
  | PositionedValue SourceSpan [Comment] Expr
  deriving (Show)

-- |
-- Metadata that tells where a let binding originated
--
data WhereProvenance
  -- |
  -- The let binding was originally a where clause
  --
  = FromWhere
  -- |
  -- The let binding was always a let binding
  --
  | FromLet
  deriving (Show)

-- |
-- An alternative in a case statement
--
data CaseAlternative = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder]
    -- |
    -- The result expression or a collect of guarded expressions
    --
  , caseAlternativeResult :: [GuardedExpr]
  } deriving (Show)

-- |
-- A statement in a do-notation block
--
data DoNotationElement
  -- |
  -- A monadic value without a binder
  --
  = DoNotationValue Expr
  -- |
  -- A monadic value with a binder
  --
  | DoNotationBind Binder Expr
  -- |
  -- A let statement, i.e. a pure value with a binder
  --
  | DoNotationLet [Declaration]
  -- |
  -- A do notation element with source position information
  --
  | PositionedDoNotationElement SourceSpan [Comment] DoNotationElement
  deriving (Show)


-- For a record update such as:
--
--  x { foo = 0
--    , bar { baz = 1
--          , qux = 2 } }
--
-- We represent the updates as the `PathTree`:
--
--  [ ("foo", Leaf 3)
--  , ("bar", Branch [ ("baz", Leaf 1)
--                   , ("qux", Leaf 2) ]) ]
--
-- Which we then convert to an expression representing the following:
--
--   let x' = x
--   in x' { foo = 0
--         , bar = x'.bar { baz = 1
--                        , qux = 2 } }
--
-- The `let` here is required to prevent re-evaluating the object expression `x`.
-- However we don't generate this when using an anonymous argument for the object.
--

newtype PathTree t = PathTree (AssocList PSString (PathNode t))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data PathNode t = Leaf t | Branch (PathTree t)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype AssocList k t = AssocList { runAssocList :: [(k, t)] }
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''DeclarationRef)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ImportDeclarationType)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExportSource)

isTrueExpr :: Expr -> Bool
isTrueExpr (Literal _ (BooleanLiteral True)) = True
isTrueExpr (Var _ (Qualified (Just (ModuleName "Prelude")) (Ident "otherwise"))) = True
isTrueExpr (Var _ (Qualified (Just (ModuleName "Data.Boolean")) (Ident "otherwise"))) = True
isTrueExpr (TypedValue _ e _) = isTrueExpr e
isTrueExpr (PositionedValue _ _ e) = isTrueExpr e
isTrueExpr _ = False
