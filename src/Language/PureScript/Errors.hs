module Language.PureScript.Errors
  ( module Language.PureScript.AST
  , module Language.PureScript.Errors
  ) where

import           Prelude

import           Control.Arrow ((&&&))
import           Control.Exception (displayException)
import           Control.Lens (both, head1, over)
import           Control.Monad
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Writer
import           Data.Bifunctor (first, second)
import           Data.Bitraversable (bitraverse)
import           Data.Char (isSpace)
import           Data.Containers.ListUtils (nubOrdOn)
import           Data.Either (partitionEithers)
import           Data.Foldable (fold)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.Functor.Identity (Identity(..))
import           Data.List (transpose, nubBy, partition, dropWhileEnd, sortOn, uncons)
import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Maybe (maybeToList, fromMaybe, isJust, mapMaybe)
import qualified Data.Map as M
import           Data.Ord (Down(..))
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Traversable (for)
import qualified GHC.Stack
import           Language.PureScript.AST
import qualified Language.PureScript.Bundle as Bundle
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Constants.Prim as C
import           Language.PureScript.Crash
import qualified Language.PureScript.CST.Errors as CST
import qualified Language.PureScript.CST.Print as CST
import           Language.PureScript.Environment
import           Language.PureScript.Label (Label(..))
import           Language.PureScript.Names
import           Language.PureScript.Pretty
import           Language.PureScript.Pretty.Common (endWith)
import           Language.PureScript.PSString (decodeStringWithReplacement)
import           Language.PureScript.Roles
import           Language.PureScript.Traversals
import           Language.PureScript.Types
import qualified Language.PureScript.Publish.BoxesHelpers as BoxHelpers
import qualified System.Console.ANSI as ANSI
import           System.FilePath (makeRelative)
import qualified Text.PrettyPrint.Boxes as Box
import           Witherable (wither)

-- | A type of error messages
data SimpleErrorMessage
  = InternalCompilerError Text Text
  | ModuleNotFound ModuleName
  | ErrorParsingFFIModule FilePath (Maybe Bundle.ErrorMessage)
  | ErrorParsingCSTModule CST.ParserError
  | WarningParsingCSTModule CST.ParserWarning
  | MissingFFIModule ModuleName
  | UnnecessaryFFIModule ModuleName FilePath
  | MissingFFIImplementations ModuleName [Ident]
  | UnusedFFIImplementations ModuleName [Ident]
  | InvalidFFIIdentifier ModuleName Text
  | DeprecatedFFIPrime ModuleName Text
  | DeprecatedFFICommonJSModule ModuleName FilePath
  | UnsupportedFFICommonJSExports ModuleName [Text]
  | UnsupportedFFICommonJSImports ModuleName [Text]
  | FileIOError Text IOError -- ^ A description of what we were trying to do, and the error which occurred
  | InfiniteType SourceType
  | InfiniteKind SourceType
  | MultipleValueOpFixities (OpName 'ValueOpName)
  | MultipleTypeOpFixities (OpName 'TypeOpName)
  | OrphanTypeDeclaration Ident
  | OrphanKindDeclaration (ProperName 'TypeName)
  | OrphanRoleDeclaration (ProperName 'TypeName)
  | RedefinedIdent Ident
  | OverlappingNamesInLet
  | UnknownName (Qualified Name)
  | UnknownImport ModuleName Name
  | UnknownImportDataConstructor ModuleName (ProperName 'TypeName) (ProperName 'ConstructorName)
  | UnknownExport Name
  | UnknownExportDataConstructor (ProperName 'TypeName) (ProperName 'ConstructorName)
  | ScopeConflict Name [ModuleName]
  | ScopeShadowing Name (Maybe ModuleName) [ModuleName]
  | DeclConflict Name Name
  | ExportConflict (Qualified Name) (Qualified Name)
  | DuplicateModule ModuleName
  | DuplicateTypeClass (ProperName 'ClassName) SourceSpan
  | DuplicateInstance Ident SourceSpan
  | DuplicateTypeArgument Text
  | InvalidDoBind
  | InvalidDoLet
  | CycleInDeclaration Ident
  | CycleInTypeSynonym (NEL.NonEmpty (ProperName 'TypeName))
  | CycleInTypeClassDeclaration (NEL.NonEmpty (Qualified (ProperName 'ClassName)))
  | CycleInKindDeclaration (NEL.NonEmpty (Qualified (ProperName 'TypeName)))
  | CycleInModules (NEL.NonEmpty ModuleName)
  | NameIsUndefined Ident
  | UndefinedTypeVariable (ProperName 'TypeName)
  | PartiallyAppliedSynonym (Qualified (ProperName 'TypeName))
  | EscapedSkolem Text (Maybe SourceSpan) SourceType
  | TypesDoNotUnify SourceType SourceType
  | KindsDoNotUnify SourceType SourceType
  | ConstrainedTypeUnified SourceType SourceType
  | OverlappingInstances (Qualified (ProperName 'ClassName)) [SourceType] [Qualified (Either SourceType Ident)]
  | NoInstanceFound
      SourceConstraint -- ^ constraint that could not be solved
      [Qualified (Either SourceType Ident)] -- ^ a list of instances that stopped further progress in instance chains due to ambiguity
      Bool -- ^ whether eliminating unknowns with annotations might help
  | AmbiguousTypeVariables SourceType [(Text, Int)]
  | UnknownClass (Qualified (ProperName 'ClassName))
  | PossiblyInfiniteInstance (Qualified (ProperName 'ClassName)) [SourceType]
  | PossiblyInfiniteCoercibleInstance
  | CannotDerive (Qualified (ProperName 'ClassName)) [SourceType]
  | InvalidDerivedInstance (Qualified (ProperName 'ClassName)) [SourceType] Int
  | ExpectedTypeConstructor (Qualified (ProperName 'ClassName)) [SourceType] SourceType
  | InvalidNewtypeInstance (Qualified (ProperName 'ClassName)) [SourceType]
  | MissingNewtypeSuperclassInstance (Qualified (ProperName 'ClassName)) (Qualified (ProperName 'ClassName)) [SourceType]
  | UnverifiableSuperclassInstance (Qualified (ProperName 'ClassName)) (Qualified (ProperName 'ClassName)) [SourceType]
  | CannotFindDerivingType (ProperName 'TypeName)
  | DuplicateLabel Label (Maybe Expr)
  | DuplicateValueDeclaration Ident
  | ArgListLengthsDiffer Ident
  | OverlappingArgNames (Maybe Ident)
  | MissingClassMember (NEL.NonEmpty (Ident, SourceType))
  | ExtraneousClassMember Ident (Qualified (ProperName 'ClassName))
  | ExpectedType SourceType SourceType
  -- | constructor name, expected argument count, actual argument count
  | IncorrectConstructorArity (Qualified (ProperName 'ConstructorName)) Int Int
  | ExprDoesNotHaveType Expr SourceType
  | PropertyIsMissing Label
  | AdditionalProperty Label
  | OrphanInstance Ident (Qualified (ProperName 'ClassName)) (S.Set ModuleName) [SourceType]
  | InvalidNewtype (ProperName 'TypeName)
  | InvalidInstanceHead SourceType
  | TransitiveExportError DeclarationRef [DeclarationRef]
  | TransitiveDctorExportError DeclarationRef [ProperName 'ConstructorName]
  | HiddenConstructors DeclarationRef (Qualified (ProperName 'ClassName))
  | ShadowedName Ident
  | ShadowedTypeVar Text
  | UnusedTypeVar Text
  | UnusedName Ident
  | UnusedDeclaration Ident
  | WildcardInferredType SourceType Context
  | HoleInferredType Text SourceType Context (Maybe TypeSearch)
  | MissingTypeDeclaration Ident SourceType
  | MissingKindDeclaration KindSignatureFor (ProperName 'TypeName) SourceType
  | OverlappingPattern [[Binder]] Bool
  | IncompleteExhaustivityCheck
  | ImportHidingModule ModuleName
  | UnusedImport ModuleName (Maybe ModuleName)
  | UnusedExplicitImport ModuleName [Name] (Maybe ModuleName) [DeclarationRef]
  | UnusedDctorImport ModuleName (ProperName 'TypeName) (Maybe ModuleName) [DeclarationRef]
  | UnusedDctorExplicitImport ModuleName (ProperName 'TypeName) [ProperName 'ConstructorName] (Maybe ModuleName) [DeclarationRef]
  | DuplicateSelectiveImport ModuleName
  | DuplicateImport ModuleName ImportDeclarationType (Maybe ModuleName)
  | DuplicateImportRef Name
  | DuplicateExportRef Name
  | IntOutOfRange Integer Text Integer Integer
  | ImplicitQualifiedImport ModuleName ModuleName [DeclarationRef]
  | ImplicitQualifiedImportReExport ModuleName ModuleName [DeclarationRef]
  | ImplicitImport ModuleName [DeclarationRef]
  | HidingImport ModuleName [DeclarationRef]
  | CaseBinderLengthDiffers Int [Binder]
  | IncorrectAnonymousArgument
  | InvalidOperatorInBinder (Qualified (OpName 'ValueOpName)) (Qualified Ident)
  | CannotGeneralizeRecursiveFunction Ident SourceType
  | CannotDeriveNewtypeForData (ProperName 'TypeName)
  | ExpectedWildcard (ProperName 'TypeName)
  | CannotUseBindWithDo Ident
  -- | instance name, type class, expected argument count, actual argument count
  | ClassInstanceArityMismatch Ident (Qualified (ProperName 'ClassName)) Int Int
  -- | a user-defined warning raised by using the Warn type class
  | UserDefinedWarning SourceType
  -- | a declaration couldn't be used because it contained free variables
  | UnusableDeclaration Ident [[Text]]
  | CannotDefinePrimModules ModuleName
  | MixedAssociativityError (NEL.NonEmpty (Qualified (OpName 'AnyOpName), Associativity))
  | NonAssociativeError (NEL.NonEmpty (Qualified (OpName 'AnyOpName)))
  | QuantificationCheckFailureInKind Text
  | QuantificationCheckFailureInType [Int] SourceType
  | VisibleQuantificationCheckFailureInType Text
  | UnsupportedTypeInKind SourceType
  -- | Declared role was more permissive than inferred.
  | RoleMismatch
      Text -- ^ Type variable in question
      Role -- ^ inferred role
      Role -- ^ declared role
  | InvalidCoercibleInstanceDeclaration [SourceType]
  | UnsupportedRoleDeclaration
  | RoleDeclarationArityMismatch (ProperName 'TypeName) Int Int
  | DuplicateRoleDeclaration (ProperName 'TypeName)
  | CannotDeriveInvalidConstructorArg (Qualified (ProperName 'ClassName))
  deriving (Show)

data ErrorMessage = ErrorMessage
  [ErrorMessageHint]
  SimpleErrorMessage
  deriving (Show)

newtype ErrorSuggestion = ErrorSuggestion Text

-- | Get the source span for an error
errorSpan :: ErrorMessage -> Maybe (NEL.NonEmpty SourceSpan)
errorSpan = findHint matchPE <> findHint matchRP
  where
  matchPE (PositionedError sss) = Just sss
  matchPE _ = Nothing
  matchRP (RelatedPositions sss) = Just sss
  matchRP _ = Nothing

-- | Get the module name for an error
errorModule :: ErrorMessage -> Maybe ModuleName
errorModule = findHint matchModule
  where
  matchModule (ErrorInModule mn) = Just mn
  matchModule _ = Nothing

findHint :: (ErrorMessageHint -> Maybe a) -> ErrorMessage -> Maybe a
findHint f (ErrorMessage hints _) = getLast . foldMap (Last . f) $ hints

-- | Remove the module name and span hints from an error
stripModuleAndSpan :: ErrorMessage -> ErrorMessage
stripModuleAndSpan (ErrorMessage hints e) = ErrorMessage (filter (not . shouldStrip) hints) e
  where
  shouldStrip (ErrorInModule _) = True
  shouldStrip (PositionedError _) = True
  shouldStrip _ = False

-- | Get the error code for a particular error type
errorCode :: ErrorMessage -> Text
errorCode em = case unwrapErrorMessage em of
  InternalCompilerError{} -> "InternalCompilerError"
  ModuleNotFound{} -> "ModuleNotFound"
  ErrorParsingFFIModule{} -> "ErrorParsingFFIModule"
  ErrorParsingCSTModule{} -> "ErrorParsingModule"
  WarningParsingCSTModule{} -> "WarningParsingModule"
  MissingFFIModule{} -> "MissingFFIModule"
  UnnecessaryFFIModule{} -> "UnnecessaryFFIModule"
  MissingFFIImplementations{} -> "MissingFFIImplementations"
  UnusedFFIImplementations{} -> "UnusedFFIImplementations"
  InvalidFFIIdentifier{} -> "InvalidFFIIdentifier"
  DeprecatedFFIPrime{} -> "DeprecatedFFIPrime"
  DeprecatedFFICommonJSModule {} -> "DeprecatedFFICommonJSModule"
  UnsupportedFFICommonJSExports {} -> "UnsupportedFFICommonJSExports"
  UnsupportedFFICommonJSImports {} -> "UnsupportedFFICommonJSImports"
  FileIOError{} -> "FileIOError"
  InfiniteType{} -> "InfiniteType"
  InfiniteKind{} -> "InfiniteKind"
  MultipleValueOpFixities{} -> "MultipleValueOpFixities"
  MultipleTypeOpFixities{} -> "MultipleTypeOpFixities"
  OrphanTypeDeclaration{} -> "OrphanTypeDeclaration"
  OrphanKindDeclaration{} -> "OrphanKindDeclaration"
  OrphanRoleDeclaration{} -> "OrphanRoleDeclaration"
  RedefinedIdent{} -> "RedefinedIdent"
  OverlappingNamesInLet -> "OverlappingNamesInLet"
  UnknownName{} -> "UnknownName"
  UnknownImport{} -> "UnknownImport"
  UnknownImportDataConstructor{} -> "UnknownImportDataConstructor"
  UnknownExport{} -> "UnknownExport"
  UnknownExportDataConstructor{} -> "UnknownExportDataConstructor"
  ScopeConflict{} -> "ScopeConflict"
  ScopeShadowing{} -> "ScopeShadowing"
  DeclConflict{} -> "DeclConflict"
  ExportConflict{} -> "ExportConflict"
  DuplicateModule{} -> "DuplicateModule"
  DuplicateTypeClass{} -> "DuplicateTypeClass"
  DuplicateInstance{} -> "DuplicateInstance"
  DuplicateTypeArgument{} -> "DuplicateTypeArgument"
  InvalidDoBind -> "InvalidDoBind"
  InvalidDoLet -> "InvalidDoLet"
  CycleInDeclaration{} -> "CycleInDeclaration"
  CycleInTypeSynonym{} -> "CycleInTypeSynonym"
  CycleInTypeClassDeclaration{} -> "CycleInTypeClassDeclaration"
  CycleInKindDeclaration{} -> "CycleInKindDeclaration"
  CycleInModules{} -> "CycleInModules"
  NameIsUndefined{} -> "NameIsUndefined"
  UndefinedTypeVariable{} -> "UndefinedTypeVariable"
  PartiallyAppliedSynonym{} -> "PartiallyAppliedSynonym"
  EscapedSkolem{} -> "EscapedSkolem"
  TypesDoNotUnify{} -> "TypesDoNotUnify"
  KindsDoNotUnify{} -> "KindsDoNotUnify"
  ConstrainedTypeUnified{} -> "ConstrainedTypeUnified"
  OverlappingInstances{} -> "OverlappingInstances"
  NoInstanceFound{} -> "NoInstanceFound"
  AmbiguousTypeVariables{} -> "AmbiguousTypeVariables"
  UnknownClass{} -> "UnknownClass"
  PossiblyInfiniteInstance{} -> "PossiblyInfiniteInstance"
  PossiblyInfiniteCoercibleInstance -> "PossiblyInfiniteCoercibleInstance"
  CannotDerive{} -> "CannotDerive"
  InvalidNewtypeInstance{} -> "InvalidNewtypeInstance"
  MissingNewtypeSuperclassInstance{} -> "MissingNewtypeSuperclassInstance"
  UnverifiableSuperclassInstance{} -> "UnverifiableSuperclassInstance"
  InvalidDerivedInstance{} -> "InvalidDerivedInstance"
  ExpectedTypeConstructor{} -> "ExpectedTypeConstructor"
  CannotFindDerivingType{} -> "CannotFindDerivingType"
  DuplicateLabel{} -> "DuplicateLabel"
  DuplicateValueDeclaration{} -> "DuplicateValueDeclaration"
  ArgListLengthsDiffer{} -> "ArgListLengthsDiffer"
  OverlappingArgNames{} -> "OverlappingArgNames"
  MissingClassMember{} -> "MissingClassMember"
  ExtraneousClassMember{} -> "ExtraneousClassMember"
  ExpectedType{} -> "ExpectedType"
  IncorrectConstructorArity{} -> "IncorrectConstructorArity"
  ExprDoesNotHaveType{} -> "ExprDoesNotHaveType"
  PropertyIsMissing{} -> "PropertyIsMissing"
  AdditionalProperty{} -> "AdditionalProperty"
  OrphanInstance{} -> "OrphanInstance"
  InvalidNewtype{} -> "InvalidNewtype"
  InvalidInstanceHead{} -> "InvalidInstanceHead"
  TransitiveExportError{} -> "TransitiveExportError"
  TransitiveDctorExportError{} -> "TransitiveDctorExportError"
  HiddenConstructors{} -> "HiddenConstructors"
  ShadowedName{} -> "ShadowedName"
  UnusedName{} -> "UnusedName"
  UnusedDeclaration{} -> "UnusedDeclaration"
  ShadowedTypeVar{} -> "ShadowedTypeVar"
  UnusedTypeVar{} -> "UnusedTypeVar"
  WildcardInferredType{} -> "WildcardInferredType"
  HoleInferredType{} -> "HoleInferredType"
  MissingTypeDeclaration{} -> "MissingTypeDeclaration"
  MissingKindDeclaration{} -> "MissingKindDeclaration"
  OverlappingPattern{} -> "OverlappingPattern"
  IncompleteExhaustivityCheck{} -> "IncompleteExhaustivityCheck"
  ImportHidingModule{} -> "ImportHidingModule"
  UnusedImport{} -> "UnusedImport"
  UnusedExplicitImport{} -> "UnusedExplicitImport"
  UnusedDctorImport{} -> "UnusedDctorImport"
  UnusedDctorExplicitImport{} -> "UnusedDctorExplicitImport"
  DuplicateSelectiveImport{} -> "DuplicateSelectiveImport"
  DuplicateImport{} -> "DuplicateImport"
  DuplicateImportRef{} -> "DuplicateImportRef"
  DuplicateExportRef{} -> "DuplicateExportRef"
  IntOutOfRange{} -> "IntOutOfRange"
  ImplicitQualifiedImport{} -> "ImplicitQualifiedImport"
  ImplicitQualifiedImportReExport{} -> "ImplicitQualifiedImportReExport"
  ImplicitImport{} -> "ImplicitImport"
  HidingImport{} -> "HidingImport"
  CaseBinderLengthDiffers{} -> "CaseBinderLengthDiffers"
  IncorrectAnonymousArgument -> "IncorrectAnonymousArgument"
  InvalidOperatorInBinder{} -> "InvalidOperatorInBinder"
  CannotGeneralizeRecursiveFunction{} -> "CannotGeneralizeRecursiveFunction"
  CannotDeriveNewtypeForData{} -> "CannotDeriveNewtypeForData"
  ExpectedWildcard{} -> "ExpectedWildcard"
  CannotUseBindWithDo{} -> "CannotUseBindWithDo"
  ClassInstanceArityMismatch{} -> "ClassInstanceArityMismatch"
  UserDefinedWarning{} -> "UserDefinedWarning"
  UnusableDeclaration{} -> "UnusableDeclaration"
  CannotDefinePrimModules{} -> "CannotDefinePrimModules"
  MixedAssociativityError{} -> "MixedAssociativityError"
  NonAssociativeError{} -> "NonAssociativeError"
  QuantificationCheckFailureInKind {} -> "QuantificationCheckFailureInKind"
  QuantificationCheckFailureInType {} -> "QuantificationCheckFailureInType"
  VisibleQuantificationCheckFailureInType {} -> "VisibleQuantificationCheckFailureInType"
  UnsupportedTypeInKind {} -> "UnsupportedTypeInKind"
  RoleMismatch {} -> "RoleMismatch"
  InvalidCoercibleInstanceDeclaration {} -> "InvalidCoercibleInstanceDeclaration"
  UnsupportedRoleDeclaration {} -> "UnsupportedRoleDeclaration"
  RoleDeclarationArityMismatch {} -> "RoleDeclarationArityMismatch"
  DuplicateRoleDeclaration {} -> "DuplicateRoleDeclaration"
  CannotDeriveInvalidConstructorArg{} -> "CannotDeriveInvalidConstructorArg"

-- | A stack trace for an error
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage]
  } deriving (Show, Semigroup, Monoid)

-- | Check whether a collection of errors is empty or not.
nonEmpty :: MultipleErrors -> Bool
nonEmpty = not . null . runMultipleErrors

-- | Create an error set from a single simple error message
errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [ErrorMessage [] err]

-- | Create an error set from a single simple error message and source annotation
errorMessage' :: SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage' ss err = MultipleErrors [ErrorMessage [positionedError ss] err]

-- | Create an error set from a single simple error message and source annotations
errorMessage'' :: NEL.NonEmpty SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage'' sss err = MultipleErrors [ErrorMessage [PositionedError sss] err]

-- | Create an error from multiple (possibly empty) source spans, reversed sorted.
errorMessage''' :: [SourceSpan] -> SimpleErrorMessage -> MultipleErrors
errorMessage''' sss err =
  maybe (errorMessage err) (flip errorMessage'' err)
    . NEL.nonEmpty
    . sortOn Down
    $ filter (/= NullSourceSpan) sss

-- | Create an error set from a single error message
singleError :: ErrorMessage -> MultipleErrors
singleError = MultipleErrors . pure

-- | Lift a function on ErrorMessage to a function on MultipleErrors
onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f = MultipleErrors . map f . runMultipleErrors

-- | Add a hint to an error message
addHint :: ErrorMessageHint -> MultipleErrors -> MultipleErrors
addHint hint = addHints [hint]

-- | Add hints to an error message
addHints :: [ErrorMessageHint] -> MultipleErrors -> MultipleErrors
addHints hints = onErrorMessages $ \(ErrorMessage hints' se) -> ErrorMessage (hints ++ hints') se

-- | A map from rigid type variable name/unknown variable pairs to new variables.
data TypeMap = TypeMap
  { umSkolemMap   :: M.Map Int (String, Int, Maybe SourceSpan)
  -- ^ a map from skolems to their new names, including source and naming info
  , umUnknownMap  :: M.Map Int Int
  -- ^ a map from unification variables to their new names
  , umNextIndex   :: Int
  -- ^ unknowns and skolems share a source of names during renaming, to
  -- avoid overlaps in error messages. This is the next label for either case.
  } deriving Show

defaultUnknownMap :: TypeMap
defaultUnknownMap = TypeMap M.empty M.empty 0

-- | How critical the issue is
data Level = Error | Warning deriving Show

-- | Extract nested error messages from wrapper errors
unwrapErrorMessage :: ErrorMessage -> SimpleErrorMessage
unwrapErrorMessage (ErrorMessage _ se) = se

replaceUnknowns :: SourceType -> State TypeMap SourceType
replaceUnknowns = everywhereOnTypesTopDownM replaceTypes where
  replaceTypes :: SourceType -> State TypeMap SourceType
  replaceTypes (TUnknown ann u) = do
    m <- get
    case M.lookup u (umUnknownMap m) of
      Nothing -> do
        let u' = umNextIndex m
        put $ m { umUnknownMap = M.insert u u' (umUnknownMap m), umNextIndex = u' + 1 }
        return (TUnknown ann u')
      Just u' -> return (TUnknown ann u')
  -- We intentionally remove the kinds from skolems, because they are never
  -- presented when pretty-printing. Any unknowns in those kinds shouldn't
  -- appear in the list of unknowns unless used somewhere else.
  replaceTypes (Skolem ann name _ s sko) = do
    m <- get
    case M.lookup s (umSkolemMap m) of
      Nothing -> do
        let s' = umNextIndex m
        put $ m { umSkolemMap = M.insert s (T.unpack name, s', Just (fst ann)) (umSkolemMap m), umNextIndex = s' + 1 }
        return (Skolem ann name Nothing s' sko)
      Just (_, s', _) -> return (Skolem ann name Nothing s' sko)
  replaceTypes other = return other

onTypesInErrorMessage :: (SourceType -> SourceType) -> ErrorMessage -> ErrorMessage
onTypesInErrorMessage f = runIdentity . onTypesInErrorMessageM (Identity . f)

onTypesInErrorMessageM :: Applicative m => (SourceType -> m SourceType) -> ErrorMessage -> m ErrorMessage
onTypesInErrorMessageM f (ErrorMessage hints simple) = ErrorMessage <$> traverse gHint hints <*> gSimple simple
  where
  gSimple (InfiniteType t) = InfiniteType <$> f t
  gSimple (TypesDoNotUnify t1 t2) = TypesDoNotUnify <$> f t1 <*> f t2
  gSimple (ConstrainedTypeUnified t1 t2) = ConstrainedTypeUnified <$> f t1 <*> f t2
  gSimple (ExprDoesNotHaveType e t) = ExprDoesNotHaveType e <$> f t
  gSimple (InvalidInstanceHead t) = InvalidInstanceHead <$> f t
  gSimple (NoInstanceFound con ambig unks) = NoInstanceFound <$> overConstraintArgs (traverse f) con <*> pure ambig <*> pure unks
  gSimple (AmbiguousTypeVariables t uis) = AmbiguousTypeVariables <$> f t <*> pure uis
  gSimple (OverlappingInstances cl ts insts) = OverlappingInstances cl <$> traverse f ts <*> traverse (traverse $ bitraverse f pure) insts
  gSimple (PossiblyInfiniteInstance cl ts) = PossiblyInfiniteInstance cl <$> traverse f ts
  gSimple (CannotDerive cl ts) = CannotDerive cl <$> traverse f ts
  gSimple (InvalidNewtypeInstance cl ts) = InvalidNewtypeInstance cl <$> traverse f ts
  gSimple (MissingNewtypeSuperclassInstance cl1 cl2 ts) = MissingNewtypeSuperclassInstance cl1 cl2 <$> traverse f ts
  gSimple (UnverifiableSuperclassInstance cl1 cl2 ts) = UnverifiableSuperclassInstance cl1 cl2 <$> traverse f ts
  gSimple (InvalidDerivedInstance cl ts n) = InvalidDerivedInstance cl <$> traverse f ts <*> pure n
  gSimple (ExpectedTypeConstructor cl ts ty) = ExpectedTypeConstructor cl <$> traverse f ts <*> f ty
  gSimple (ExpectedType ty k) = ExpectedType <$> f ty <*> pure k
  gSimple (OrphanInstance nm cl noms ts) = OrphanInstance nm cl noms <$> traverse f ts
  gSimple (WildcardInferredType ty ctx) = WildcardInferredType <$> f ty <*> traverse (sndM f) ctx
  gSimple (HoleInferredType name ty ctx env) = HoleInferredType name <$> f ty <*> traverse (sndM f) ctx  <*> traverse (onTypeSearchTypesM f) env
  gSimple (MissingTypeDeclaration nm ty) = MissingTypeDeclaration nm <$> f ty
  gSimple (MissingKindDeclaration sig nm ty) = MissingKindDeclaration sig nm <$> f ty
  gSimple (CannotGeneralizeRecursiveFunction nm ty) = CannotGeneralizeRecursiveFunction nm <$> f ty
  gSimple (InvalidCoercibleInstanceDeclaration tys) = InvalidCoercibleInstanceDeclaration <$> traverse f tys
  gSimple other = pure other

  gHint (ErrorInSubsumption t1 t2) = ErrorInSubsumption <$> f t1 <*> f t2
  gHint (ErrorUnifyingTypes t1 t2) = ErrorUnifyingTypes <$> f t1 <*> f t2
  gHint (ErrorCheckingType e t) = ErrorCheckingType e <$> f t
  gHint (ErrorCheckingKind t k) = ErrorCheckingKind <$> f t <*> f k
  gHint (ErrorInferringKind t) = ErrorInferringKind <$> f t
  gHint (ErrorInApplication e1 t1 e2) = ErrorInApplication e1 <$> f t1 <*> pure e2
  gHint (ErrorInInstance cl ts) = ErrorInInstance cl <$> traverse f ts
  gHint (ErrorSolvingConstraint con) = ErrorSolvingConstraint <$> overConstraintArgs (traverse f) con
  gHint other = pure other

errorDocUri :: ErrorMessage -> Text
errorDocUri e = "https://github.com/purescript/documentation/blob/master/errors/" <> errorCode e <> ".md"

-- TODO Other possible suggestions:
-- WildcardInferredType - source span not small enough
-- DuplicateSelectiveImport - would require 2 ranges to remove and 1 insert
errorSuggestion :: SimpleErrorMessage -> Maybe ErrorSuggestion
errorSuggestion err =
    case err of
      UnusedImport{} -> emptySuggestion
      DuplicateImport{} -> emptySuggestion
      UnusedExplicitImport mn _ qual refs -> suggest $ importSuggestion mn refs qual
      UnusedDctorImport mn _ qual refs -> suggest $ importSuggestion mn refs qual
      UnusedDctorExplicitImport mn _ _ qual refs -> suggest $ importSuggestion mn refs qual
      ImplicitImport mn refs -> suggest $ importSuggestion mn refs Nothing
      ImplicitQualifiedImport mn asModule refs -> suggest $ importSuggestion mn refs (Just asModule)
      ImplicitQualifiedImportReExport mn asModule refs -> suggest $ importSuggestion mn refs (Just asModule)
      HidingImport mn refs -> suggest $ importSuggestion mn refs Nothing
      MissingTypeDeclaration ident ty -> suggest $ showIdent ident <> " :: " <> T.pack (prettyPrintSuggestedTypeSimplified ty) <> "\n"
      MissingKindDeclaration sig name ty -> suggest $ prettyPrintKindSignatureFor sig <> " " <> runProperName name <> " :: " <> T.pack (prettyPrintSuggestedTypeSimplified ty) <> "\n"
      WildcardInferredType ty _ -> suggest $ T.pack (prettyPrintSuggestedTypeSimplified ty)
      WarningParsingCSTModule pe -> do
        let toks = CST.errToks pe
        case CST.errType pe of
          CST.WarnDeprecatedRowSyntax -> do
            let kind = CST.printTokens $ drop 1 toks
                sugg | " " `T.isPrefixOf` kind = "Row" <> kind
                     | otherwise = "Row " <> kind
            suggest sugg
          CST.WarnDeprecatedForeignKindSyntax -> suggest $ "data " <> CST.printTokens (drop 3 toks)
          CST.WarnDeprecatedKindImportSyntax -> suggest $ CST.printTokens $ drop 1 toks
          CST.WarnDeprecatedKindExportSyntax -> suggest $ CST.printTokens $ drop 1 toks
          CST.WarnDeprecatedCaseOfOffsideSyntax -> Nothing
      _ -> Nothing
  where
    emptySuggestion = Just $ ErrorSuggestion ""
    suggest = Just . ErrorSuggestion

    importSuggestion :: ModuleName -> [ DeclarationRef ] -> Maybe ModuleName -> Text
    importSuggestion mn refs qual =
      "import " <> runModuleName mn <> " (" <> T.intercalate ", " (mapMaybe prettyPrintRef refs) <> ")" <> qstr qual

    qstr :: Maybe ModuleName -> Text
    qstr (Just mn) = " as " <> runModuleName mn
    qstr Nothing = ""

suggestionSpan :: ErrorMessage -> Maybe SourceSpan
suggestionSpan e =
  -- The `NEL.head` is a bit arbitrary here, but I don't think we'll
  -- have errors-with-suggestions that also have multiple source
  -- spans. -garyb
  getSpan (unwrapErrorMessage e) . NEL.head <$> errorSpan e
  where
    startOnly SourceSpan{spanName, spanStart} = SourceSpan {spanName, spanStart, spanEnd = spanStart}

    getSpan simple ss =
      case simple of
        MissingTypeDeclaration{} -> startOnly ss
        MissingKindDeclaration{} -> startOnly ss
        _ -> ss

showSuggestion :: SimpleErrorMessage -> Text
showSuggestion suggestion = case errorSuggestion suggestion of
  Just (ErrorSuggestion x) -> x
  _ -> ""

ansiColor :: (ANSI.ColorIntensity, ANSI.Color) -> String
ansiColor (intensity, color) =
   ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color]

ansiColorReset :: String
ansiColorReset =
   ANSI.setSGRCode [ANSI.Reset]

colorCode :: Maybe (ANSI.ColorIntensity, ANSI.Color) -> Text -> Text
colorCode codeColor code = case codeColor of
  Nothing -> code
  Just cc -> T.pack (ansiColor cc) <> code <> T.pack ansiColorReset

colorCodeBox :: Maybe (ANSI.ColorIntensity, ANSI.Color) -> Box.Box -> Box.Box
colorCodeBox codeColor b = case codeColor of
  Nothing -> b
  Just cc
    | Box.rows b == 1 ->
        Box.text (ansiColor cc) Box.<> b `endWith` Box.text ansiColorReset

    | otherwise -> Box.hcat Box.left -- making two boxes, one for each side of the box so that it will set each row it's own color and will reset it afterwards
        [ Box.vcat Box.top $ replicate (Box.rows b) $ Box.text $ ansiColor cc
        , b
        , Box.vcat Box.top $ replicate (Box.rows b) $ Box.text ansiColorReset
        ]

-- | Default color intensity and color for code
defaultCodeColor :: (ANSI.ColorIntensity, ANSI.Color)
defaultCodeColor = (ANSI.Dull, ANSI.Yellow)

-- | `prettyPrintSingleError` Options
data PPEOptions = PPEOptions
  { ppeCodeColor         :: Maybe (ANSI.ColorIntensity, ANSI.Color) -- ^ Color code with this color... or not
  , ppeFull              :: Bool -- ^ Should write a full error message?
  , ppeLevel             :: Level -- ^ Should this report an error or a warning?
  , ppeShowDocs          :: Bool -- ^ Should show a link to error message's doc page?
  , ppeRelativeDirectory :: FilePath -- ^ FilePath to which the errors are relative
  , ppeFileContents      :: [(FilePath, Text)] -- ^ Unparsed contents of source files
  }

-- | Default options for PPEOptions
defaultPPEOptions :: PPEOptions
defaultPPEOptions = PPEOptions
  { ppeCodeColor         = Just defaultCodeColor
  , ppeFull              = False
  , ppeLevel             = Error
  , ppeShowDocs          = True
  , ppeRelativeDirectory = mempty
  , ppeFileContents      = []
  }

-- | Pretty print a single error, simplifying if necessary
prettyPrintSingleError :: PPEOptions -> ErrorMessage -> Box.Box
prettyPrintSingleError (PPEOptions codeColor full level showDocs relPath fileContents) e = flip evalState defaultUnknownMap $ do
  em <- onTypesInErrorMessageM replaceUnknowns (if full then e else simplifyErrorMessage e)
  um <- get
  return (prettyPrintErrorMessage um em)
  where
  (markCode, markCodeBox) = (colorCode &&& colorCodeBox) codeColor

  -- Pretty print an ErrorMessage
  prettyPrintErrorMessage :: TypeMap -> ErrorMessage -> Box.Box
  prettyPrintErrorMessage typeMap (ErrorMessage hints simple) =
    paras $
      [ foldr renderHint (indent (renderSimpleErrorMessage simple)) hints
      ] ++
      maybe [] (return . Box.moveDown 1) typeInformation ++
      [ Box.moveDown 1 $ paras
          [ line $ "See " <> errorDocUri e <> " for more information, "
          , line $ "or to contribute content related to this " <> levelText <> "."
          ]
      | showDocs
      ]
    where
    typeInformation :: Maybe Box.Box
    typeInformation | not (null types) = Just $ Box.hsep 1 Box.left [ line "where", paras types ]
                    | otherwise = Nothing
      where
      types :: [Box.Box]
      types = map skolemInfo  (M.elems (umSkolemMap typeMap)) ++
              map unknownInfo (M.elems (umUnknownMap typeMap))

      skolemInfo :: (String, Int, Maybe SourceSpan) -> Box.Box
      skolemInfo (name, s, ss) =
        paras $
          line (markCode (T.pack (name <> show s)) <> " is a rigid type variable")
          : foldMap (return . line . ("  bound at " <>) . displayStartEndPos) ss

      unknownInfo :: Int -> Box.Box
      unknownInfo u = line $ markCode ("t" <> T.pack (show u)) <> " is an unknown type"

    renderSimpleErrorMessage :: SimpleErrorMessage -> Box.Box
    renderSimpleErrorMessage (InternalCompilerError ctx err) =
      paras [ line "Internal compiler error:"
            , indent $ line err
            , line ctx
            , line "Please report this at https://github.com/purescript/purescript/issues"
            ]
    renderSimpleErrorMessage (ModuleNotFound mn) =
      paras [ line $ "Module " <> markCode (runModuleName mn) <> " was not found."
            , line $
                if isBuiltinModuleName mn
                  then
                    "Module names in the Prim namespace are reserved for built-in modules, but this version of the compiler does not provide module " <> markCode (runModuleName mn) <> ". You may be able to fix this by updating your compiler to a newer version."
                  else
                    "Make sure the source file exists, and that it has been provided as an input to the compiler."
            ]
    renderSimpleErrorMessage (FileIOError doWhat err) =
      paras [ line $ "I/O error while trying to " <> doWhat
            , indent . lineS $ displayException err
            ]
    renderSimpleErrorMessage (ErrorParsingFFIModule path extra) =
      paras $ [ line "Unable to parse foreign module:"
              , indent . lineS $ path
              ] ++
              map (indent . lineS) (concatMap Bundle.printErrorMessage (maybeToList extra))
    renderSimpleErrorMessage (ErrorParsingCSTModule err) =
      paras [ line "Unable to parse module: "
            , line $ T.pack $ CST.prettyPrintErrorMessage err
            ]
    renderSimpleErrorMessage (WarningParsingCSTModule err) =
      paras [ line $ T.pack $ CST.prettyPrintWarningMessage err
            ]
    renderSimpleErrorMessage (MissingFFIModule mn) =
      line $ "The foreign module implementation for module " <> markCode (runModuleName mn) <> " is missing."
    renderSimpleErrorMessage (UnnecessaryFFIModule mn path) =
      paras [ line $ "An unnecessary foreign module implementation was provided for module " <> markCode (runModuleName mn) <> ": "
            , indent . lineS $ path
            , line $ "Module " <> markCode (runModuleName mn) <> " does not contain any foreign import declarations, so a foreign module is not necessary."
            ]
    renderSimpleErrorMessage (MissingFFIImplementations mn idents) =
      paras [ line $ "The following values are not defined in the foreign module for module " <> markCode (runModuleName mn) <> ": "
            , indent . paras $ map (line . runIdent) idents
            ]
    renderSimpleErrorMessage (UnusedFFIImplementations mn idents) =
      paras [ line $ "The following definitions in the foreign module for module " <> markCode (runModuleName mn) <> " are unused: "
            , indent . paras $ map (line . runIdent) idents
            ]
    renderSimpleErrorMessage (InvalidFFIIdentifier mn ident) =
      paras [ line $ "In the FFI module for " <> markCode (runModuleName mn) <> ":"
            , indent . paras $
                [ line $ "The identifier " <> markCode ident <> " is not valid in PureScript."
                , line "Note that exported identifiers in FFI modules must be valid PureScript identifiers."
                ]
            ]
    renderSimpleErrorMessage (DeprecatedFFIPrime mn ident) =
      paras [ line $ "In the FFI module for " <> markCode (runModuleName mn) <> ":"
            , indent . paras $
                [ line $ "The identifier " <> markCode ident <> " contains a prime (" <> markCode "'" <> ")."
                , line "Primes are not allowed in identifiers exported from FFI modules."
                ]
            ]
    renderSimpleErrorMessage (DeprecatedFFICommonJSModule mn path) =
      paras [ line $ "A CommonJS foreign module implementation was provided for module " <> markCode (runModuleName mn) <> ": "
            , indent . lineS $ path
            , line "CommonJS foreign modules are no longer supported. Use native JavaScript/ECMAScript module syntax instead."
            ]
    renderSimpleErrorMessage (UnsupportedFFICommonJSExports mn idents) =
      paras [ line $ "The following CommonJS exports are not supported in the ES foreign module for module " <> markCode (runModuleName mn) <> ": "
            , indent . paras $ map line idents
            ]
    renderSimpleErrorMessage (UnsupportedFFICommonJSImports mn mids) =
      paras [ line $ "The following CommonJS imports are not supported in the ES foreign module for module " <> markCode (runModuleName mn) <> ": "
            , indent . paras $ map line mids
            ]
    renderSimpleErrorMessage InvalidDoBind =
      line "The last statement in a 'do' block must be an expression, but this block ends with a binder."
    renderSimpleErrorMessage InvalidDoLet =
      line "The last statement in a 'do' block must be an expression, but this block ends with a let binding."
    renderSimpleErrorMessage OverlappingNamesInLet =
      line "The same name was used more than once in a let binding."
    renderSimpleErrorMessage (InfiniteType ty) =
      paras [ line "An infinite type was inferred for an expression: "
            , markCodeBox $ indent $ prettyType ty
            ]
    renderSimpleErrorMessage (InfiniteKind ki) =
      paras [ line "An infinite kind was inferred for a type: "
            , markCodeBox $ indent $ prettyType ki
            ]
    renderSimpleErrorMessage (MultipleValueOpFixities op) =
      line $ "There are multiple fixity/precedence declarations for operator " <> markCode (showOp op)
    renderSimpleErrorMessage (MultipleTypeOpFixities op) =
      line $ "There are multiple fixity/precedence declarations for type operator " <> markCode (showOp op)
    renderSimpleErrorMessage (OrphanTypeDeclaration nm) =
      line $ "The type declaration for " <> markCode (showIdent nm) <> " should be followed by its definition."
    renderSimpleErrorMessage (OrphanKindDeclaration nm) =
      line $ "The kind declaration for " <> markCode (runProperName nm) <> " should be followed by its definition."
    renderSimpleErrorMessage (OrphanRoleDeclaration nm) =
      line $ "The role declaration for " <> markCode (runProperName nm) <> " should follow its definition."
    renderSimpleErrorMessage (RedefinedIdent name) =
      line $ "The value " <> markCode (showIdent name) <> " has been defined multiple times"
    renderSimpleErrorMessage (UnknownName name@(Qualified (BySourcePos _) (IdentName (Ident i)))) | i `elem` [ C.bind, C.discard ] =
      line $ "Unknown " <> printName name <> ". You're probably using do-notation, which the compiler replaces with calls to the " <> markCode "bind" <> " and " <> markCode "discard" <> " functions. Please import " <> markCode i <> " from module " <> markCode "Prelude"
    renderSimpleErrorMessage (UnknownName name@(Qualified (BySourcePos _) (IdentName (Ident i)))) | i == C.negate =
      line $ "Unknown " <> printName name <> ". You're probably using numeric negation (the unary " <> markCode "-" <> " operator), which the compiler replaces with calls to the " <> markCode i <> " function. Please import " <> markCode i <> " from module " <> markCode "Prelude"
    renderSimpleErrorMessage (UnknownName name) =
      line $ "Unknown " <> printName name
    renderSimpleErrorMessage (UnknownImport mn name) =
      paras [ line $ "Cannot import " <> printName (Qualified ByNullSourcePos name) <> " from module " <> markCode (runModuleName mn)
            , line "It either does not exist or the module does not export it."
            ]
    renderSimpleErrorMessage (UnknownImportDataConstructor mn tcon dcon) =
      line $ "Module " <> runModuleName mn <> " does not export data constructor " <> markCode (runProperName dcon) <> " for type " <> markCode (runProperName tcon)
    renderSimpleErrorMessage (UnknownExport name) =
      line $ "Cannot export unknown " <> printName (Qualified ByNullSourcePos name)
    renderSimpleErrorMessage (UnknownExportDataConstructor tcon dcon) =
      line $ "Cannot export data constructor " <> markCode (runProperName dcon) <> " for type " <> markCode (runProperName tcon) <> ", as it has not been declared."
    renderSimpleErrorMessage (ScopeConflict nm ms) =
      paras [ line $ "Conflicting definitions are in scope for " <> printName (Qualified ByNullSourcePos nm) <> " from the following modules:"
            , indent $ paras $ map (line . markCode . runModuleName) ms
            ]
    renderSimpleErrorMessage (ScopeShadowing nm exmn ms) =
      paras [ line $ "Shadowed definitions are in scope for " <> printName (Qualified ByNullSourcePos nm) <> " from the following open imports:"
            , indent $ paras $ map (line . markCode . ("import " <>) . runModuleName) ms
            , line $ "These will be ignored and the " <> case exmn of
                Just exmn' -> "declaration from " <> markCode (runModuleName exmn') <> " will be used."
                Nothing -> "local declaration will be used."
            ]
    renderSimpleErrorMessage (DeclConflict new existing) =
      line $ "Declaration for " <> printName (Qualified ByNullSourcePos new) <> " conflicts with an existing " <> nameType existing <> " of the same name."
    renderSimpleErrorMessage (ExportConflict new existing) =
      line $ "Export for " <> printName new <> " conflicts with " <> printName existing
    renderSimpleErrorMessage (DuplicateModule mn) =
      line $ "Module " <> markCode (runModuleName mn) <> " has been defined multiple times"
    renderSimpleErrorMessage (DuplicateTypeClass pn ss) =
      paras [ line ("Type class " <> markCode (runProperName pn) <> " has been defined multiple times:")
            , indent $ line $ displaySourceSpan relPath ss
            ]
    renderSimpleErrorMessage (DuplicateInstance pn ss) =
      paras [ line ("Instance " <> markCode (showIdent pn) <> " has been defined multiple times:")
            , indent $ line $ displaySourceSpan relPath ss
            ]
    renderSimpleErrorMessage (CycleInDeclaration nm) =
      line $ "The value of " <> markCode (showIdent nm) <> " is undefined here, so this reference is not allowed."
    renderSimpleErrorMessage (CycleInModules mns) =
      case mns of
        mn :| [] ->
          line $ "Module " <> markCode (runModuleName mn) <> " imports itself."
        _ ->
          paras [ line "There is a cycle in module dependencies in these modules: "
                , indent $ paras (line . markCode . runModuleName <$> NEL.toList mns)
                ]
    renderSimpleErrorMessage (CycleInTypeSynonym names) =
      paras $ cycleError <>
            [ line "Cycles are disallowed because they can lead to loops in the type checker."
            , line "Consider using a 'newtype' instead."
            ]
      where
      cycleError = case names of
        pn :| [] -> pure . line $ "A cycle appears in the definition of type synonym " <> markCode (runProperName pn)
        _ -> [ line " A cycle appears in a set of type synonym definitions:"
             , indent $ line $ "{" <> T.intercalate ", " (markCode . runProperName <$> NEL.toList names) <> "}"
             ]
    renderSimpleErrorMessage (CycleInTypeClassDeclaration (name :| [])) =
      paras [ line $ "A type class '" <> markCode (runProperName (disqualify name)) <> "' may not have itself as a superclass." ]
    renderSimpleErrorMessage (CycleInTypeClassDeclaration names) =
      paras [ line "A cycle appears in a set of type class definitions:"
            , indent $ line $ "{" <> T.intercalate ", " (markCode . runProperName . disqualify <$> NEL.toList names) <> "}"
            , line "Cycles are disallowed because they can lead to loops in the type checker."
            ]
    renderSimpleErrorMessage (CycleInKindDeclaration (name :| [])) =
      paras [ line $ "A kind declaration '" <> markCode (runProperName (disqualify name)) <> "' may not refer to itself in its own signature." ]
    renderSimpleErrorMessage (CycleInKindDeclaration names) =
      paras [ line "A cycle appears in a set of kind declarations:"
            , indent $ line $ "{" <> T.intercalate ", " (markCode . runProperName . disqualify <$> NEL.toList names) <> "}"
            , line "Kind declarations may not refer to themselves in their own signatures."
            ]
    renderSimpleErrorMessage (NameIsUndefined ident) =
      line $ "Value " <> markCode (showIdent ident) <> " is undefined."
    renderSimpleErrorMessage (UndefinedTypeVariable name) =
      line $ "Type variable " <> markCode (runProperName name) <> " is undefined."
    renderSimpleErrorMessage (PartiallyAppliedSynonym name) =
      paras [ line $ "Type synonym " <> markCode (showQualified runProperName name) <> " is partially applied."
            , line "Type synonyms must be applied to all of their type arguments."
            ]
    renderSimpleErrorMessage (EscapedSkolem name Nothing ty) =
      paras [ line $ "The type variable " <> markCode name <> " has escaped its scope, appearing in the type"
            , markCodeBox $ indent $ prettyType ty
            ]
    renderSimpleErrorMessage (EscapedSkolem name (Just srcSpan) ty) =
      paras [ line $ "The type variable " <> markCode name <> ", bound at"
            , indent $ line $ displaySourceSpan relPath srcSpan
            , line "has escaped its scope, appearing in the type"
            , markCodeBox $ indent $ prettyType ty
            ]
    renderSimpleErrorMessage (TypesDoNotUnify u1 u2)
      = let (row1Box, row2Box) = printRows u1 u2

        in paras [ line "Could not match type"
                 , row1Box
                 , line "with type"
                 , row2Box
                 ]

    renderSimpleErrorMessage (KindsDoNotUnify k1 k2) =
      paras [ line "Could not match kind"
            , markCodeBox $ indent $ prettyType k1
            , line "with kind"
            , markCodeBox $ indent $ prettyType k2
            ]
    renderSimpleErrorMessage (ConstrainedTypeUnified t1 t2) =
      paras [ line "Could not match constrained type"
            , markCodeBox $ indent $ prettyType t1
            , line "with type"
            , markCodeBox $ indent $ prettyType t2
            ]
    renderSimpleErrorMessage (OverlappingInstances _ _ []) = internalError "OverlappingInstances: empty instance list"
    renderSimpleErrorMessage (OverlappingInstances nm ts ds) =
      paras [ line "Overlapping type class instances found for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line "The following instances were found:"
            , indent $ paras (map prettyInstanceName ds)
            ]
    renderSimpleErrorMessage (UnknownClass nm) =
      paras [ line "No type class instance was found for class"
            , markCodeBox $ indent $ line (showQualified runProperName nm)
            , line "because the class was not in scope. Perhaps it was not exported."
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint _ C.Fail _ [ ty ] _) _ _) | Just box <- toTypelevelString ty =
      paras [ line "A custom type error occurred while solving type class constraints:"
            , indent box
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint _ C.Partial
                                                          _
                                                          _
                                                          (Just (PartialConstraintData bs b))) _ _) =
      paras [ line "A case expression could not be determined to cover all inputs."
            , line "The following additional cases are required to cover all inputs:"
            , indent $ paras $
                Box.hsep 1 Box.left
                  (map (paras . map (line . markCode)) (transpose bs))
                  : [line "..." | not b]
            , line "Alternatively, add a Partial constraint to the type of the enclosing value."
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint _ C.Discard _ [ty] _) _ _) =
      paras [ line "A result of type"
            , markCodeBox $ indent $ prettyType ty
            , line "was implicitly discarded in a do notation block."
            , line ("You can use " <> markCode "_ <- ..." <> " to explicitly discard the result.")
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint _ nm _ ts _) ambiguous unks) =
      paras [ line "No type class instance was found for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , paras $ let useMessage msg =
                            [ line msg
                            , indent $ paras (map prettyInstanceName ambiguous)
                            ]
                      in case ambiguous of
                        [] -> []
                        [_] -> useMessage "The following instance partially overlaps the above constraint, which means the rest of its instance chain will not be considered:"
                        _ -> useMessage "The following instances partially overlap the above constraint, which means the rest of their instance chains will not be considered:"
            , paras [ line "The instance head contains unknown type variables. Consider adding a type annotation."
                    | unks
                    ]
            ]
    renderSimpleErrorMessage (AmbiguousTypeVariables t uis) =
      paras [ line "The inferred type"
            , markCodeBox $ indent $ prettyType t
            , line "has type variables which are not determined by those mentioned in the body of the type:"
            , indent $ Box.hsep 1 Box.left
              [ Box.vcat Box.left
                [ line $ markCode (u <> T.pack (show i)) <> " could not be determined"
                | (u, i) <- uis ]
              ]
            , line "Consider adding a type annotation."
            ]
    renderSimpleErrorMessage (PossiblyInfiniteInstance nm ts) =
      paras [ line "Type class instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line "is possibly infinite."
            ]
    renderSimpleErrorMessage PossiblyInfiniteCoercibleInstance =
      line $ "A " <> markCode "Coercible" <> " instance is possibly infinite."
    renderSimpleErrorMessage (CannotDerive nm ts) =
      paras [ line "Cannot derive a type class instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line "since instances of this type class are not derivable."
            ]
    renderSimpleErrorMessage (InvalidNewtypeInstance nm ts) =
      paras [ line "Cannot derive newtype instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line "Make sure this is a newtype."
            ]
    renderSimpleErrorMessage (MissingNewtypeSuperclassInstance su cl ts) =
      paras [ line "The derived newtype instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName cl)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line $ "does not include a derived superclass instance for " <> markCode (showQualified runProperName su) <> "."
            ]
    renderSimpleErrorMessage (UnverifiableSuperclassInstance su cl ts) =
      paras [ line "The derived newtype instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName cl)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line $ "implies an superclass instance for " <> markCode (showQualified runProperName su) <> " which could not be verified."
            ]
    renderSimpleErrorMessage (InvalidDerivedInstance nm ts argCount) =
      paras [ line "Cannot derive the type class instance"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , line $ fold
                [ "because the "
                , markCode (showQualified runProperName nm)
                , " type class has "
                , T.pack (show argCount)
                , " type "
                , if argCount == 1 then "argument" else "arguments"
                , ", but the declaration specifies " <> T.pack (show (length ts)) <> "."
                ]
            ]
    renderSimpleErrorMessage (ExpectedTypeConstructor nm ts ty) =
      paras [ line "Cannot derive the type class instance"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , "because the type"
            , markCodeBox $ indent $ prettyType ty
            , line "is not of the required form T a_1 ... a_n, where T is a type constructor defined in the same module."
            ]
    renderSimpleErrorMessage (CannotFindDerivingType nm) =
      line $ "Cannot derive a type class instance, because the type declaration for " <> markCode (runProperName nm) <> " could not be found."
    renderSimpleErrorMessage (DuplicateLabel l expr) =
      paras $ [ line $ "Label " <> markCode (prettyPrintLabel l) <> " appears more than once in a row type." ]
                       <> foldMap (\expr' -> [ line "Relevant expression: "
                                             , markCodeBox $ indent $ prettyPrintValue prettyDepth expr'
                                             ]) expr
    renderSimpleErrorMessage (DuplicateTypeArgument name) =
      line $ "Type argument " <> markCode name <> " appears more than once."
    renderSimpleErrorMessage (DuplicateValueDeclaration nm) =
      line $ "Multiple value declarations exist for " <> markCode (showIdent nm) <> "."
    renderSimpleErrorMessage (ArgListLengthsDiffer ident) =
      line $ "Argument list lengths differ in declaration " <> markCode (showIdent ident)
    renderSimpleErrorMessage (OverlappingArgNames ident) =
      line $ "Overlapping names in function/binder" <> foldMap ((" in declaration " <>) . showIdent) ident
    renderSimpleErrorMessage (MissingClassMember identsAndTypes) =
      paras [ line "The following type class members have not been implemented:"
            , Box.vcat Box.left
              [ markCodeBox $ Box.text (T.unpack (showIdent ident)) Box.<> " :: " Box.<> prettyType ty
              | (ident, ty) <- NEL.toList identsAndTypes ]
            ]
    renderSimpleErrorMessage (ExtraneousClassMember ident className) =
      line $ "" <> markCode (showIdent ident) <> " is not a member of type class " <> markCode (showQualified runProperName className)
    renderSimpleErrorMessage (ExpectedType ty kind) =
      paras [ line $ "In a type-annotated expression " <> markCode "x :: t" <> ", the type " <> markCode "t" <> " must have kind " <> markCode C.typ <> "."
            , line "The error arises from the type"
            , markCodeBox $ indent $ prettyType ty
            , line "having the kind"
            , markCodeBox $ indent $ prettyType kind
            , line "instead."
            ]
    renderSimpleErrorMessage (IncorrectConstructorArity nm expected actual) =
      paras [ line $ "Data constructor " <> markCode (showQualified runProperName nm) <> " was given " <> T.pack (show actual) <> " arguments in a case expression, but expected " <> T.pack (show expected) <> " arguments."
            , line $ "This problem can be fixed by giving " <> markCode (showQualified runProperName nm) <> " " <> T.pack (show expected) <> " arguments."
            ]
    renderSimpleErrorMessage (ExprDoesNotHaveType expr ty) =
      paras [ line "Expression"
            , markCodeBox $ indent $ prettyPrintValue prettyDepth expr
            , line "does not have type"
            , markCodeBox $ indent $ prettyType ty
            ]
    renderSimpleErrorMessage (PropertyIsMissing prop) =
      line $ "Type of expression lacks required label " <> markCode (prettyPrintLabel prop) <> "."
    renderSimpleErrorMessage (AdditionalProperty prop) =
      line $ "Type of expression contains additional label " <> markCode (prettyPrintLabel prop) <> "."
    renderSimpleErrorMessage (OrphanInstance nm cnm nonOrphanModules ts) =
      paras [ line $ "Orphan instance" <> prettyPrintPlainIdent nm <> " found for "
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName cnm)
                , Box.vcat Box.left (map prettyTypeAtom ts)
                ]
            , Box.vcat Box.left $ case modulesToList of
                [] -> [ line "There is nowhere this instance can be placed without being an orphan."
                      , line "A newtype wrapper can be used to avoid this problem."
                      ]
                _  -> [ Box.text $ "This problem can be resolved by declaring the instance in "
                          <> T.unpack formattedModules
                          <> ", or by defining the instance on a newtype wrapper."
                      ]
                ]
      where
        modulesToList = S.toList $ S.delete (moduleNameFromString "Prim") nonOrphanModules
        formattedModules = T.intercalate " or " (markCode . runModuleName <$> modulesToList)
    renderSimpleErrorMessage (InvalidNewtype name) =
      paras [ line $ "Newtype " <> markCode (runProperName name) <> " is invalid."
            , line "Newtypes must define a single constructor with a single argument."
            ]
    renderSimpleErrorMessage (InvalidInstanceHead ty) =
      paras [ line "Type class instance head is invalid due to use of type"
            , markCodeBox $ indent $ prettyType ty
            , line "All types appearing in instance declarations must be of the form T a_1 .. a_n, where each type a_i is of the same form, unless the type is fully determined by other type class arguments via functional dependencies."
            ]
    renderSimpleErrorMessage (TransitiveExportError x ys) =
      paras [ line $ "An export for " <> markCode (prettyPrintExport x) <> " requires the following to also be exported: "
            , indent $ paras $ map (line . markCode . prettyPrintExport) ys
            ]
    renderSimpleErrorMessage (TransitiveDctorExportError x ctors) =
      paras [ line $ "An export for " <> markCode (prettyPrintExport x) <> " requires the following data constructor" <> (if length ctors == 1 then "" else "s") <> " to also be exported: "
            , indent $ paras $ map (line . markCode . runProperName) ctors
            ]
    renderSimpleErrorMessage (HiddenConstructors x className) =
      paras [ line $ "An export for " <> markCode (prettyPrintExport x) <> " hides data constructors but the type declares an instance of " <> markCode (showQualified runProperName className) <> "."
            , line "Such instance allows to match and construct values of this type, effectively making the constructors public."
            ]
    renderSimpleErrorMessage (ShadowedName nm) =
      line $ "Name " <> markCode (showIdent nm) <> " was shadowed."
    renderSimpleErrorMessage (ShadowedTypeVar tv) =
      line $ "Type variable " <> markCode tv <> " was shadowed."
    renderSimpleErrorMessage (UnusedName nm) =
      line $ "Name " <> markCode (showIdent nm) <> " was introduced but not used."
    renderSimpleErrorMessage (UnusedDeclaration nm) =
      line $ "Declaration " <> markCode (showIdent nm) <> " was not used, and is not exported."
    renderSimpleErrorMessage (UnusedTypeVar tv) =
      line $ "Type variable " <> markCode tv <> " is ambiguous, since it is unused in the polymorphic type which introduces it."
    renderSimpleErrorMessage (ImportHidingModule name) =
      paras [ line "hiding imports cannot be used to hide modules."
            , line $ "An attempt was made to hide the import of " <> markCode (runModuleName name)
            ]
    renderSimpleErrorMessage (WildcardInferredType ty ctx) =
      paras $ [ line "Wildcard type definition has the inferred type "
              , markCodeBox $ indent $ prettyType ty
              ] <> renderContext ctx
    renderSimpleErrorMessage (HoleInferredType name ty ctx ts) =
      let
        maxTSResults = 15
        tsResult = case ts of
          Just TSAfter{tsAfterIdentifiers=idents} | not (null idents) ->
            let
              formatTS (names, types) =
                let
                  idBoxes = Box.text . T.unpack . showQualified id <$> names
                  tyBoxes = (\t -> BoxHelpers.indented
                              (Box.text ":: " Box.<> prettyType t)) <$> types
                  longestId = maximum (map Box.cols idBoxes)
                in
                  Box.vcat Box.top $
                      zipWith (Box.<>)
                      (Box.alignHoriz Box.left longestId <$> idBoxes)
                      tyBoxes
            in [ line "You could substitute the hole with one of these values:"
               , markCodeBox (indent (formatTS (unzip (take maxTSResults idents))))
               ]
          _ -> []
      in
        paras $ [ line $ "Hole '" <> markCode name <> "' has the inferred type "
                , markCodeBox (indent (prettyTypeWithDepth maxBound ty))
                ] ++ tsResult ++ renderContext ctx
    renderSimpleErrorMessage (MissingTypeDeclaration ident ty) =
      paras [ line $ "No type declaration was provided for the top-level declaration of " <> markCode (showIdent ident) <> "."
            , line "It is good practice to provide type declarations as a form of documentation."
            , line $ "The inferred type of " <> markCode (showIdent ident) <> " was:"
            , markCodeBox $ indent $ prettyTypeWithDepth maxBound ty
            ]
    renderSimpleErrorMessage (MissingKindDeclaration sig name ty) =
      let sigKw = prettyPrintKindSignatureFor sig in
      paras [ line $ "The inferred kind for the " <> sigKw <> " declaration " <> markCode (runProperName name) <> " contains polymorphic kinds."
            , line "Consider adding a top-level kind signature as a form of documentation."
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line $ sigKw <> " " <> runProperName name <> " ::"
                , prettyTypeWithDepth maxBound ty
                ]
            ]
    renderSimpleErrorMessage (OverlappingPattern bs b) =
      paras $ [ line "A case expression contains unreachable cases:\n"
              , Box.hsep 1 Box.left (map (paras . map (line . prettyPrintBinderAtom)) (transpose bs))
              ] ++
              [ line "..." | not b ]
    renderSimpleErrorMessage IncompleteExhaustivityCheck =
      paras [ line "An exhaustivity check was abandoned due to too many possible cases."
            , line "You may want to decompose your data types into smaller types."
            ]

    renderSimpleErrorMessage (UnusedImport mn qualifier) =
      let
        mark = markCode . runModuleName
        unqualified = "The import of " <> mark mn <> " is redundant"
        msg' q = "The qualified import of " <> mark mn <> " as " <> mark q <> " is redundant"
        msg = maybe unqualified msg'
      in line $ msg qualifier

    renderSimpleErrorMessage msg@(UnusedExplicitImport mn names _ _) =
      paras [ line $ "The import of module " <> markCode (runModuleName mn) <> " contains the following unused references:"
            , indent $ paras $ map (line . markCode . runName . Qualified ByNullSourcePos) names
            , line "It could be replaced with:"
            , indent $ line $ markCode $ showSuggestion msg ]

    renderSimpleErrorMessage msg@(UnusedDctorImport mn name _ _) =
      paras [line $ "The import of type " <> markCode (runProperName name)
                    <> " from module " <> markCode (runModuleName mn) <> " includes data constructors but only the type is used"
            , line "It could be replaced with:"
            , indent $ line $ markCode $ showSuggestion msg ]

    renderSimpleErrorMessage msg@(UnusedDctorExplicitImport mn name names _ _) =
      paras [ line $ "The import of type " <> markCode (runProperName name)
                     <> " from module " <> markCode (runModuleName mn) <> " includes the following unused data constructors:"
            , indent $ paras $ map (line . markCode . runProperName) names
            , line "It could be replaced with:"
            , indent $ line $ markCode $ showSuggestion msg ]

    renderSimpleErrorMessage (DuplicateSelectiveImport name) =
      line $ "There is an existing import of " <> markCode (runModuleName name) <> ", consider merging the import lists"

    renderSimpleErrorMessage (DuplicateImport name imp qual) =
      line $ "Duplicate import of " <> markCode (prettyPrintImport name imp qual)

    renderSimpleErrorMessage (DuplicateImportRef name) =
      line $ "Import list contains multiple references to " <> printName (Qualified ByNullSourcePos name)

    renderSimpleErrorMessage (DuplicateExportRef name) =
      line $ "Export list contains multiple references to " <> printName (Qualified ByNullSourcePos name)

    renderSimpleErrorMessage (IntOutOfRange value backend lo hi) =
      paras [ line $ "Integer value " <> markCode (T.pack (show value)) <> " is out of range for the " <> backend <> " backend."
            , line $ "Acceptable values fall within the range " <> markCode (T.pack (show lo)) <> " to " <> markCode (T.pack (show hi)) <> " (inclusive)." ]

    renderSimpleErrorMessage msg@(ImplicitQualifiedImport importedModule asModule _) =
      paras [ line $ "Module " <> markCode (runModuleName importedModule) <> " was imported as " <> markCode (runModuleName asModule) <> " with unspecified imports."
            , line $ "As there are multiple modules being imported as " <> markCode (runModuleName asModule) <> ", consider using the explicit form:"
            , indent $ line $ markCode $ showSuggestion msg
            ]
    renderSimpleErrorMessage msg@(ImplicitQualifiedImportReExport importedModule asModule _) =
      paras [ line $ "Module " <> markCode (runModuleName importedModule) <> " was imported as " <> markCode (runModuleName asModule) <> " with unspecified imports."
            , line "As this module is being re-exported, consider using the explicit form:"
            , indent $ line $ markCode $ showSuggestion msg
            ]

    renderSimpleErrorMessage msg@(ImplicitImport mn _) =
      paras [ line $ "Module " <> markCode (runModuleName mn) <> " has unspecified imports, consider using the explicit form: "
            , indent $ line $ markCode $ showSuggestion msg
            ]

    renderSimpleErrorMessage msg@(HidingImport mn _) =
      paras [ line $ "Module " <> markCode (runModuleName mn) <> " has unspecified imports, consider using the inclusive form: "
            , indent $ line $ markCode $ showSuggestion msg
            ]

    renderSimpleErrorMessage (CaseBinderLengthDiffers l bs) =
      paras [ line "Binder list length differs in case alternative:"
            , indent $ line $ T.intercalate ", " $ fmap prettyPrintBinderAtom bs
            , line $ "Expecting " <> T.pack (show l) <> " binder" <> (if l == 1 then "" else "s") <> "."
            ]

    renderSimpleErrorMessage IncorrectAnonymousArgument =
      line "An anonymous function argument appears in an invalid context."

    renderSimpleErrorMessage (InvalidOperatorInBinder op fn) =
      paras [ line $ "Operator " <> markCode (showQualified showOp op) <> " cannot be used in a pattern as it is an alias for function " <> showQualified showIdent fn <> "."
            , line "Only aliases for data constructors may be used in patterns."
            ]

    renderSimpleErrorMessage (CannotGeneralizeRecursiveFunction ident ty) =
      paras [ line $ "Unable to generalize the type of the recursive function " <> markCode (showIdent ident) <> "."
            , line $ "The inferred type of " <> markCode (showIdent ident) <> " was:"
            , markCodeBox $ indent $ prettyType ty
            , line "Try adding a type signature."
            ]

    renderSimpleErrorMessage (CannotDeriveNewtypeForData tyName) =
      paras [ line $ "Cannot derive an instance of the " <> markCode "Newtype" <> " class for non-newtype " <> markCode (runProperName tyName) <> "."
            ]

    renderSimpleErrorMessage (ExpectedWildcard tyName) =
      paras [ line $ "Expected a type wildcard (_) when deriving an instance for " <> markCode (runProperName tyName) <> "."
            ]

    renderSimpleErrorMessage (CannotUseBindWithDo name) =
      paras [ line $ "The name " <> markCode (showIdent name) <> " cannot be brought into scope in a do notation block, since do notation uses the same name."
            ]

    renderSimpleErrorMessage (ClassInstanceArityMismatch dictName className expected actual) =
      paras [ line $ "The type class " <> markCode (showQualified runProperName className) <>
                     " expects " <> T.pack (show expected) <> " " <> argsMsg <> "."
            , line $ "But the instance" <> prettyPrintPlainIdent dictName <> mismatchMsg <> T.pack (show actual) <> "."
            ]
        where
          mismatchMsg = if actual > expected then " provided " else " only provided "
          argsMsg = if expected > 1 then "arguments" else "argument"

    renderSimpleErrorMessage (UserDefinedWarning msgTy) =
      let msg = fromMaybe (prettyType msgTy) (toTypelevelString msgTy) in
      paras [ line "A custom warning occurred while solving type class constraints:"
            , indent msg
            ]

    renderSimpleErrorMessage (UnusableDeclaration ident unexplained) =
      paras $
        [ line $ "The declaration " <> markCode (showIdent ident) <> " contains arguments that couldn't be determined."
        ] <>

        case unexplained of
          [required] ->
            [ line $ "These arguments are: { " <> T.intercalate ", " required <> " }"
            ]

          options  ->
            [ line "To fix this, one of the following sets of variables must be determined:"
            , Box.moveRight 2 . Box.vsep 0 Box.top $
                map (\set -> line $ "{ " <> T.intercalate ", " set <> " }") options
            ]

    renderSimpleErrorMessage (CannotDefinePrimModules mn) =
      paras
        [ line $ "The module name " <> markCode (runModuleName mn) <> " is in the Prim namespace."
        , line "The Prim namespace is reserved for compiler-defined terms."
        ]

    renderSimpleErrorMessage (MixedAssociativityError opsWithAssoc) =
      paras
        [ line "Cannot parse an expression that uses operators of the same precedence but mixed associativity:"
        , indent $ paras $ map (\(name, assoc) -> line $ markCode (showQualified showOp name) <> " is " <> markCode (T.pack (showAssoc assoc))) (NEL.toList opsWithAssoc)
        , line "Use parentheses to resolve this ambiguity."
        ]

    renderSimpleErrorMessage (NonAssociativeError ops) =
      if NEL.length ops == 1
        then
          paras
            [ line $ "Cannot parse an expression that uses multiple instances of the non-associative operator " <> markCode (showQualified showOp (NEL.head ops)) <> "."
            , line "Use parentheses to resolve this ambiguity."
            ]
        else
          paras
            [ line "Cannot parse an expression that uses multiple non-associative operators of the same precedence:"
            , indent $ paras $ map (line . markCode . showQualified showOp) (NEL.toList ops)
            , line "Use parentheses to resolve this ambiguity."
            ]

    renderSimpleErrorMessage (QuantificationCheckFailureInKind var) =
      paras
        [ line $ "Cannot generalize the kind of type variable " <> markCode var <> " since it would not be well-scoped."
        , line "Try adding a kind annotation."
        ]

    renderSimpleErrorMessage (QuantificationCheckFailureInType us ty) =
      let unks =
            fmap (\u -> Box.hsep 1 Box.top [ "where"
                                           , markCodeBox (prettyType (srcTUnknown u))
                                           , "is an unknown kind."
                                           ]) us
      in paras
           [ line "Cannot unambiguously generalize kinds appearing in the elaborated type:"
           , indent $ markCodeBox $ typeAsBox prettyDepth ty
           , paras unks
           , line "Try adding additional kind signatures or polymorphic kind variables."
           ]

    renderSimpleErrorMessage (VisibleQuantificationCheckFailureInType var) =
      paras
        [ line $ "Visible dependent quantification of type variable " <> markCode var <> " is not supported."
        , line "If you would like this feature supported, please bother Liam Goodacre (@LiamGoodacre)."
        ]

    renderSimpleErrorMessage (UnsupportedTypeInKind ty) =
      paras
        [ line "The type:"
        , indent $ markCodeBox $ prettyType ty
        , line "is not supported in kinds."
        ]

    renderSimpleErrorMessage (RoleMismatch var inferred declared) =
      paras
        [ line $ "Role mismatch for the type parameter " <> markCode var <> ":"
        , indent . line $
            "The annotation says " <> markCode (displayRole declared) <>
            " but the role " <> markCode (displayRole inferred) <>
            " is required."
        ]

    renderSimpleErrorMessage (InvalidCoercibleInstanceDeclaration tys) =
      paras
        [ line "Invalid type class instance declaration for"
        , markCodeBox $ indent $ Box.hsep 1 Box.left
            [ line (showQualified runProperName C.Coercible)
            , Box.vcat Box.left (map prettyTypeAtom tys)
            ]
        , line "Instance declarations of this type class are disallowed."
        ]

    renderSimpleErrorMessage UnsupportedRoleDeclaration =
      line "Role declarations are only supported for data types, not for type synonyms nor type classes."

    renderSimpleErrorMessage (RoleDeclarationArityMismatch name expected actual) =
      line $ T.intercalate " "
        [ "The type"
        , markCode (runProperName name)
        , "expects"
        , T.pack (show expected)
        , if expected == 1 then "argument" else "arguments"
        , "but its role declaration lists"
            <> if actual > expected then "" else " only"
        , T.pack (show actual)
        , if actual > 1 then "roles" else "role"
        ] <> "."

    renderSimpleErrorMessage (DuplicateRoleDeclaration name) =
      line $ "Duplicate role declaration for " <> markCode (runProperName name) <> "."

    renderSimpleErrorMessage (CannotDeriveInvalidConstructorArg className) =
      paras
        [ line $ "One or more type variables are in positions that prevent " <> markCode (runProperName $ disqualify className) <> " from being derived."
        , line $ "To derive this class, make sure that these variables are only used as the final arguments to type constructors, "
          <> "and that those type constructors themselves have instances of " <> markCode (runProperName $ disqualify className) <> "."
        ]

    renderHint :: ErrorMessageHint -> Box.Box -> Box.Box
    renderHint (ErrorUnifyingTypes t1@RCons{} t2@RCons{}) detail =
      let (row1Box, row2Box) = printRows t1 t2
      in paras [ detail
            , Box.hsep 1 Box.top [ line "while trying to match type"
                                 , row1Box
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "with type"
                                                   , row2Box
                                                   ]
            ]
    renderHint (ErrorUnifyingTypes t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while trying to match type"
                                 , markCodeBox $ typeAsBox prettyDepth t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "with type"
                                                   , markCodeBox $ typeAsBox prettyDepth t2
                                                   ]
            ]
    renderHint (ErrorInExpression expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ Box.text "in the expression"
                                 , markCodeBox $ markCodeBox $ prettyPrintValue prettyDepth expr
                                 ]
            ]
    renderHint (ErrorInModule mn) detail =
      paras [ line $ "in module " <> markCode (runModuleName mn)
            , detail
            ]
    renderHint (ErrorInSubsumption t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that type"
                                 , markCodeBox $ typeAsBox prettyDepth t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "is at least as general as type"
                                                   , markCodeBox $ typeAsBox prettyDepth t2
                                                   ]
            ]
    renderHint (ErrorInInstance nm ts) detail =
      paras [ detail
            , line "in type class instance"
            , markCodeBox $ indent $ Box.hsep 1 Box.top
               [ line $ showQualified runProperName nm
               , Box.vcat Box.left (map (typeAtomAsBox prettyDepth) ts)
               ]
            ]
    renderHint (ErrorCheckingKind ty kd) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that type"
                                 , markCodeBox $ typeAsBox prettyDepth ty
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has kind"
                                                   , markCodeBox $ typeAsBox prettyDepth kd
                                                   ]
            ]
    renderHint (ErrorInferringKind ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the kind of"
                                 , markCodeBox $ typeAsBox prettyDepth ty
                                 ]
            ]
    renderHint ErrorCheckingGuard detail =
      paras [ detail
            , line "while checking the type of a guard clause"
            ]
    renderHint (ErrorInferringType expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the type of"
                                 , markCodeBox $ prettyPrintValue prettyDepth expr
                                 ]
            ]
    renderHint (ErrorCheckingType expr ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that expression"
                                 , markCodeBox $ prettyPrintValue prettyDepth expr
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has type"
                                                   , markCodeBox $ typeAsBox prettyDepth ty
                                                   ]
            ]
    renderHint (ErrorCheckingAccessor expr prop) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking type of property accessor"
                                 , markCodeBox $ prettyPrintValue prettyDepth (Accessor prop expr)
                                 ]
            ]
    renderHint (ErrorInApplication f t a) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while applying a function"
                                 , markCodeBox $ prettyPrintValue prettyDepth f
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "of type"
                                                   , markCodeBox $ typeAsBox prettyDepth t
                                                   ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "to argument"
                                                   , markCodeBox $ prettyPrintValue prettyDepth a
                                                   ]
            ]
    renderHint (ErrorInDataConstructor nm) detail =
      paras [ detail
            , line $ "in data constructor " <> markCode (runProperName nm)
            ]
    renderHint (ErrorInTypeConstructor nm) detail =
      paras [ detail
            , line $ "in type constructor " <> markCode (runProperName nm)
            ]
    renderHint (ErrorInBindingGroup nms) detail =
      paras [ detail
            , line $ "in binding group " <> T.intercalate ", " (NEL.toList (fmap showIdent nms))
            ]
    renderHint (ErrorInDataBindingGroup nms) detail =
      paras [ detail
            , line $ "in data binding group " <> T.intercalate ", " (map runProperName nms)
            ]
    renderHint (ErrorInTypeSynonym name) detail =
      paras [ detail
            , line $ "in type synonym " <> markCode (runProperName name)
            ]
    renderHint (ErrorInValueDeclaration n) detail =
      paras [ detail
            , line $ "in value declaration " <> markCode (showIdent n)
            ]
    renderHint (ErrorInTypeDeclaration n) detail =
      paras [ detail
            , line $ "in type declaration for " <> markCode (showIdent n)
            ]
    renderHint (ErrorInTypeClassDeclaration name) detail =
      paras [ detail
            , line $ "in type class declaration for " <> markCode (runProperName name)
            ]
    renderHint (ErrorInKindDeclaration name) detail =
      paras [ detail
            , line $ "in kind declaration for " <> markCode (runProperName name)
            ]
    renderHint (ErrorInRoleDeclaration name) detail =
      paras [ detail
            , line $ "in role declaration for " <> markCode (runProperName name)
            ]
    renderHint (ErrorInForeignImport nm) detail =
      paras [ detail
            , line $ "in foreign import " <> markCode (showIdent nm)
            ]
    renderHint (ErrorInForeignImportData nm) detail =
      paras [ detail
            , line $ "in foreign data type declaration for " <> markCode (runProperName nm)
            ]
    renderHint (ErrorSolvingConstraint (Constraint _ nm _ ts _)) detail =
      paras [ detail
            , line "while solving type class constraint"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map (typeAtomAsBox prettyDepth) ts)
                ]
            ]
    renderHint (MissingConstructorImportForCoercible name) detail =
      paras
        [ detail
        , Box.moveUp 1 $ Box.moveRight 2 $ line $ "Solving this instance requires the newtype constructor " <> markCode (showQualified runProperName name) <> " to be in scope."
        ]
    renderHint (PositionedError srcSpan) detail =
      paras [ line $ "at " <> displaySourceSpan relPath (NEL.head srcSpan)
            , detail
            ]
    renderHint (RelatedPositions srcSpans) detail =
      paras
        [ detail
        , Box.moveRight 2 $ showSourceSpansInContext srcSpans
        ]

    printRow :: (Int -> Type a -> Box.Box) -> Type a -> Box.Box
    printRow f = markCodeBox . indent . f prettyDepth .
      if full then id else eraseForAllKindAnnotations . eraseKindApps

    -- If both rows are not empty, print them as diffs
    -- If verbose print all rows else only print unique rows
    printRows :: Type a -> Type a -> (Box.Box, Box.Box)
    printRows r1 r2 = case (full, r1, r2) of
      (True, _ , _) -> (printRow typeAsBox r1, printRow typeAsBox r2)

      (_, RCons{}, RCons{}) ->
        let (sorted1, sorted2) = filterRows (rowToList r1) (rowToList r2)
        in (printRow typeDiffAsBox sorted1, printRow typeDiffAsBox sorted2)

      (_, _, _) -> (printRow typeAsBox r1, printRow typeAsBox r2)


    -- Keep the unique labels only
    filterRows :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> (Type a, Type a)
    filterRows (s1, r1) (s2, r2) =
         let sort' = sortOn $ \(RowListItem _ name ty) -> (name, ty)
             (unique1, unique2) = diffSortedRowLists (sort' s1, sort' s2)
          in ( rowFromList (unique1, r1)
             , rowFromList (unique2, r2)
             )

    -- Importantly, this removes exactly the same number of elements from
    -- both lists, even if there are repeated (name, ty) keys. It requires
    -- the inputs to be sorted but ensures that the outputs remain sorted.
    diffSortedRowLists :: ([RowListItem a], [RowListItem a]) -> ([RowListItem a], [RowListItem a])
    diffSortedRowLists = go where
      go = \case
        (s1@(h1@(RowListItem _ name1 ty1) : t1), s2@(h2@(RowListItem _ name2 ty2) : t2)) ->
          case (name1, ty1) `compare` (name2, ty2) of
            EQ ->                go (t1, t2)
            LT -> first  (h1:) $ go (t1, s2)
            GT -> second (h2:) $ go (s1, t2)
        other -> other

    renderContext :: Context -> [Box.Box]
    renderContext [] = []
    renderContext ctx =
      [ line "in the following context:"
      , indent $ paras
          [ Box.hcat Box.left [ Box.text (T.unpack (showIdent ident) ++ " :: ")
                              , markCodeBox $ typeAsBox prettyDepth ty'
                              ]
          | (ident, ty') <- take 30 ctx
          ]
      ]

    printName :: Qualified Name -> Text
    printName qn = nameType (disqualify qn) <> " " <> markCode (runName qn)

    nameType :: Name -> Text
    nameType (IdentName _) = "value"
    nameType (ValOpName _) = "operator"
    nameType (TyName _) = "type"
    nameType (TyOpName _) = "type operator"
    nameType (DctorName _) = "data constructor"
    nameType (TyClassName _) = "type class"
    nameType (ModName _) = "module"

    runName :: Qualified Name -> Text
    runName (Qualified qb (IdentName name)) =
      showQualified showIdent (Qualified qb name)
    runName (Qualified qb (ValOpName op)) =
      showQualified showOp (Qualified qb op)
    runName (Qualified qb (TyName name)) =
      showQualified runProperName (Qualified qb name)
    runName (Qualified qb (TyOpName op)) =
      showQualified showOp (Qualified qb op)
    runName (Qualified qb (DctorName name)) =
      showQualified runProperName (Qualified qb name)
    runName (Qualified qb (TyClassName name)) =
      showQualified runProperName (Qualified qb name)
    runName (Qualified (BySourcePos _) (ModName name)) =
      runModuleName name
    runName (Qualified _ ModName{}) =
      internalError "qualified ModName in runName"

  prettyDepth :: Int
  prettyDepth | full = 1000
              | otherwise = 3

  prettyType :: Type a -> Box.Box
  prettyType = prettyTypeWithDepth prettyDepth

  prettyTypeWithDepth :: Int -> Type a -> Box.Box
  prettyTypeWithDepth depth
    | full = typeAsBox depth
    | otherwise = typeAsBox depth . eraseForAllKindAnnotations . eraseKindApps

  prettyTypeAtom :: Type a -> Box.Box
  prettyTypeAtom
    | full = typeAtomAsBox prettyDepth
    | otherwise = typeAtomAsBox prettyDepth . eraseForAllKindAnnotations . eraseKindApps

  levelText :: Text
  levelText = case level of
    Error -> "error"
    Warning -> "warning"

  paras :: forall f. Foldable f => f Box.Box -> Box.Box
  paras = Box.vcat Box.left

  -- | Simplify an error message
  simplifyErrorMessage :: ErrorMessage -> ErrorMessage
  simplifyErrorMessage (ErrorMessage hints simple) = ErrorMessage (simplifyHints hints) simple
    where
    -- Take the last instance of each "hint category"
    simplifyHints :: [ErrorMessageHint] -> [ErrorMessageHint]
    simplifyHints = reverse . nubBy categoriesEqual . stripRedundantHints simple . reverse

    -- Don't remove hints in the "other" category
    categoriesEqual :: ErrorMessageHint -> ErrorMessageHint -> Bool
    categoriesEqual x y =
      case (hintCategory x, hintCategory y) of
        (OtherHint, _) -> False
        (_, OtherHint) -> False
        (c1, c2) -> c1 == c2

    -- | See https://github.com/purescript/purescript/issues/1802
    stripRedundantHints :: SimpleErrorMessage -> [ErrorMessageHint] -> [ErrorMessageHint]
    stripRedundantHints ExprDoesNotHaveType{} = stripFirst isCheckHint
      where
      isCheckHint ErrorCheckingType{} = True
      isCheckHint _ = False
    stripRedundantHints TypesDoNotUnify{} = stripFirst isUnifyHint
      where
      isUnifyHint ErrorUnifyingTypes{} = True
      isUnifyHint _ = False
    stripRedundantHints (NoInstanceFound (Constraint _ C.Coercible _ args _) _ _) = filter (not . isSolverHint)
      where
      isSolverHint (ErrorSolvingConstraint (Constraint _ C.Coercible _ args' _)) = args == args'
      isSolverHint _ = False
    stripRedundantHints NoInstanceFound{} = stripFirst isSolverHint
      where
      isSolverHint ErrorSolvingConstraint{} = True
      isSolverHint _ = False
    stripRedundantHints _ = id

    stripFirst :: (ErrorMessageHint -> Bool) -> [ErrorMessageHint] -> [ErrorMessageHint]
    stripFirst p (PositionedError pos : hs) = PositionedError pos : stripFirst p hs
    stripFirst p (ErrorInModule mn    : hs) = ErrorInModule mn    : stripFirst p hs
    stripFirst p (hint                : hs)
      | p hint = hs
      | otherwise = hint : hs
    stripFirst _ [] = []

  hintCategory :: ErrorMessageHint -> HintCategory
  hintCategory ErrorCheckingType{}                  = ExprHint
  hintCategory ErrorInferringType{}                 = ExprHint
  hintCategory ErrorInExpression{}                  = ExprHint
  hintCategory ErrorUnifyingTypes{}                 = CheckHint
  hintCategory ErrorInSubsumption{}                 = CheckHint
  hintCategory ErrorInApplication{}                 = CheckHint
  hintCategory ErrorCheckingKind{}                  = CheckHint
  hintCategory ErrorSolvingConstraint{}             = SolverHint
  hintCategory PositionedError{}                    = PositionHint
  hintCategory ErrorInDataConstructor{}             = DeclarationHint
  hintCategory ErrorInTypeConstructor{}             = DeclarationHint
  hintCategory ErrorInBindingGroup{}                = DeclarationHint
  hintCategory ErrorInDataBindingGroup{}            = DeclarationHint
  hintCategory ErrorInTypeSynonym{}                 = DeclarationHint
  hintCategory ErrorInValueDeclaration{}            = DeclarationHint
  hintCategory ErrorInTypeDeclaration{}             = DeclarationHint
  hintCategory ErrorInTypeClassDeclaration{}        = DeclarationHint
  hintCategory ErrorInKindDeclaration{}             = DeclarationHint
  hintCategory ErrorInRoleDeclaration{}             = DeclarationHint
  hintCategory ErrorInForeignImport{}               = DeclarationHint
  hintCategory _                                    = OtherHint

  prettyPrintPlainIdent :: Ident -> Text
  prettyPrintPlainIdent ident =
    if isPlainIdent ident
    then " " <> markCode (showIdent ident)
    else ""

  prettyInstanceName :: Qualified (Either SourceType Ident) -> Box.Box
  prettyInstanceName = \case
    Qualified qb (Left ty) ->
      "instance "
        Box.<> (case qb of
                  ByModuleName mn -> "in module "
                    Box.<> line (markCode $ runModuleName mn)
                    Box.<> " "
                  _ -> Box.nullBox)
        Box.<> "with type "
        Box.<> markCodeBox (prettyType ty)
        Box.<> " "
        Box.<> (line . displayStartEndPos . fst $ getAnnForType ty)
    Qualified mn (Right inst) -> line . markCode . showQualified showIdent $ Qualified mn inst

  -- | As of this writing, this function assumes that all provided SourceSpans
  -- are non-overlapping (except for exact duplicates) and span no line breaks. A
  -- more sophisticated implementation without this limitation would be possible
  -- but isn't yet needed.
  showSourceSpansInContext :: NonEmpty SourceSpan -> Box.Box
  showSourceSpansInContext
    = maybe Box.nullBox (paras . fmap renderFile . NEL.groupWith1 spanName . NEL.sort)
    . NEL.nonEmpty
    . NEL.filter ((> 0) . sourcePosLine . spanStart)
    where
    renderFile :: NonEmpty SourceSpan -> Box.Box
    renderFile sss = maybe Box.nullBox (linesToBox . T.lines) $ lookup fileName fileContents
      where
      fileName = spanName $ NEL.head sss
      header = lineS . (<> ":") . makeRelative relPath $ fileName
      lineBlocks = makeLineBlocks $ NEL.groupWith1 (sourcePosLine . spanStart) sss

      linesToBox fileLines = Box.moveUp 1 $ header Box.// body
        where
        body
          = Box.punctuateV Box.left (lineNumberStyle "...")
          . map (paras . fmap renderLine)
          . flip evalState (fileLines, 1)
          . traverse (wither (\(i, x) -> fmap (i, , x) <$> ascLookupInState i) . NEL.toList)
          $ NEL.toList lineBlocks

    makeLineBlocks :: NonEmpty (NonEmpty SourceSpan) -> NonEmpty (NonEmpty (Int, [SourceSpan]))
    makeLineBlocks = startBlock
      where
      startBlock (h :| t) = over head1 (NEL.cons (pred $ headLineNumber h, [])) $ continueBlock h t

      continueBlock :: NonEmpty SourceSpan -> [NonEmpty SourceSpan] -> NonEmpty (NonEmpty (Int, [SourceSpan]))
      continueBlock lineGroup = \case
        [] ->
          endBlock lineGroup []
        nextGroup : groups -> case pred $ ((-) `on` headLineNumber) nextGroup lineGroup of
          n | n <= 3 ->
            over head1 (appendExtraLines n lineGroup <>) $ continueBlock nextGroup groups
          _ ->
            endBlock lineGroup . NEL.toList . startBlock $ nextGroup :| groups

      endBlock :: NonEmpty SourceSpan -> [NonEmpty (Int, [SourceSpan])] -> NonEmpty (NonEmpty (Int, [SourceSpan]))
      endBlock h t = appendExtraLines 1 h :| t

      headLineNumber = sourcePosLine . spanStart . NEL.head

      appendExtraLines :: Int -> NonEmpty SourceSpan -> NonEmpty (Int, [SourceSpan])
      appendExtraLines n lineGroup = (lineNum, NEL.toList lineGroup) :| [(lineNum + i, []) | i <- [1..n]]
        where
        lineNum = headLineNumber lineGroup

    renderLine :: (Int, Text, [SourceSpan]) -> Box.Box
    renderLine (lineNum, text, sss) = numBox Box.<+> lineBox
      where
      colSpans = nubOrdOn fst $ map (over both (pred . sourcePosColumn) . (spanStart &&& spanEnd)) sss
      numBox = lineNumberStyle $ show lineNum
      lineBox =
        if isJust codeColor
        then colorCodeBox codeColor $ line $ foldr highlightSpan text colSpans
        else line text Box.// line (finishUnderline $ foldr underlineSpan (T.length text, "") colSpans)

    highlightSpan :: (Int, Int) -> Text -> Text
    highlightSpan (startCol, endCol) text
       = prefix
      <> T.pack (ANSI.setSGRCode [ANSI.SetSwapForegroundBackground True])
      <> spanText
      <> T.pack (ANSI.setSGRCode [ANSI.SetSwapForegroundBackground False])
      <> suffix
      where
      (prefix, rest) = T.splitAt startCol text
      (spanText, suffix) = T.splitAt (endCol - startCol) rest

    underlineSpan :: (Int, Int) -> (Int, Text) -> (Int, Text)
    underlineSpan (startCol, endCol) (len, accum) = (startCol, T.replicate (endCol - startCol) "^" <> T.replicate (len - endCol) " " <> accum)

    finishUnderline :: (Int, Text) -> Text
    finishUnderline (len, accum) = T.replicate len " " <> accum

    lineNumberStyle :: String -> Box.Box
    lineNumberStyle = colorCodeBox (codeColor $> (ANSI.Vivid, ANSI.Black)) . Box.alignHoriz Box.right 5 . lineS

  -- | Lookup the nth element of a list, but without retraversing the list every
  -- time, by instead keeping a tail of the list and the current element number
  -- in State. Only works if the argument provided is strictly ascending over
  -- the life of the State.
  ascLookupInState :: forall a. Int -> State ([a], Int) (Maybe a)
  ascLookupInState j = get >>= \(as, i) -> for (uncons $ drop (j - i) as) $ \(a, as') -> put (as', succ j) $> a

-- Pretty print and export declaration
prettyPrintExport :: DeclarationRef -> Text
prettyPrintExport (TypeRef _ pn _) = runProperName pn
prettyPrintExport ref =
  fromMaybe
    (internalError "prettyPrintRef returned Nothing in prettyPrintExport")
    (prettyPrintRef ref)

prettyPrintImport :: ModuleName -> ImportDeclarationType -> Maybe ModuleName -> Text
prettyPrintImport mn idt qual =
  let i = case idt of
            Implicit -> runModuleName mn
            Explicit refs -> runModuleName mn <> " (" <> T.intercalate ", " (mapMaybe prettyPrintRef refs) <> ")"
            Hiding refs -> runModuleName mn <> " hiding (" <> T.intercalate ", " (mapMaybe prettyPrintRef refs) <> ")"
  in i <> maybe "" (\q -> " as " <> runModuleName q) qual

prettyPrintRef :: DeclarationRef -> Maybe Text
prettyPrintRef (TypeRef _ pn Nothing) =
  Just $ runProperName pn <> "(..)"
prettyPrintRef (TypeRef _ pn (Just [])) =
  Just $ runProperName pn
prettyPrintRef (TypeRef _ pn (Just dctors)) =
  Just $ runProperName pn <> "(" <> T.intercalate ", " (map runProperName dctors) <> ")"
prettyPrintRef (TypeOpRef _ op) =
  Just $ "type " <> showOp op
prettyPrintRef (ValueRef _ ident) =
  Just $ showIdent ident
prettyPrintRef (ValueOpRef _ op) =
  Just $ showOp op
prettyPrintRef (TypeClassRef _ pn) =
  Just $ "class " <> runProperName pn
prettyPrintRef (TypeInstanceRef _ ident UserNamed) =
  Just $ showIdent ident
prettyPrintRef (TypeInstanceRef _ _ CompilerNamed) =
  Nothing
prettyPrintRef (ModuleRef _ name) =
  Just $ "module " <> runModuleName name
prettyPrintRef ReExportRef{} =
  Nothing

prettyPrintKindSignatureFor :: KindSignatureFor -> Text
prettyPrintKindSignatureFor DataSig = "data"
prettyPrintKindSignatureFor NewtypeSig = "newtype"
prettyPrintKindSignatureFor TypeSynonymSig = "type"
prettyPrintKindSignatureFor ClassSig = "class"

prettyPrintSuggestedTypeSimplified :: Type a -> String
prettyPrintSuggestedTypeSimplified = prettyPrintSuggestedType . eraseForAllKindAnnotations . eraseKindApps

-- | Pretty print multiple errors
prettyPrintMultipleErrors :: PPEOptions -> MultipleErrors -> String
prettyPrintMultipleErrors ppeOptions = unlines . map renderBox . prettyPrintMultipleErrorsBox ppeOptions

-- | Pretty print multiple warnings
prettyPrintMultipleWarnings :: PPEOptions -> MultipleErrors -> String
prettyPrintMultipleWarnings ppeOptions = unlines . map renderBox . prettyPrintMultipleWarningsBox ppeOptions

-- | Pretty print warnings as a Box
prettyPrintMultipleWarningsBox :: PPEOptions -> MultipleErrors -> [Box.Box]
prettyPrintMultipleWarningsBox ppeOptions = prettyPrintMultipleErrorsWith (ppeOptions { ppeLevel = Warning }) "Warning found:" "Warning"

-- | Pretty print errors as a Box
prettyPrintMultipleErrorsBox :: PPEOptions -> MultipleErrors -> [Box.Box]
prettyPrintMultipleErrorsBox ppeOptions = prettyPrintMultipleErrorsWith (ppeOptions { ppeLevel = Error }) "Error found:" "Error"

prettyPrintMultipleErrorsWith :: PPEOptions -> String -> String -> MultipleErrors -> [Box.Box]
prettyPrintMultipleErrorsWith ppeOptions intro _ (MultipleErrors [e]) =
  let result = prettyPrintSingleError ppeOptions e
  in [ Box.vcat Box.left [ Box.text intro
                         , result
                         ]
     ]
prettyPrintMultipleErrorsWith ppeOptions _ intro (MultipleErrors es) =
  let result = map (prettyPrintSingleError ppeOptions) es
  in concat $ zipWith withIntro [1 :: Int ..] result
  where
  withIntro i err = [ Box.text (intro ++ " " ++ show i ++ " of " ++ show (length es) ++ ":")
                    , Box.moveRight 2 err
                    ]

-- | Indent to the right, and pad on top and bottom.
indent :: Box.Box -> Box.Box
indent = Box.moveUp 1 . Box.moveDown 1 . Box.moveRight 2

line :: Text -> Box.Box
line = Box.text . T.unpack

lineS :: String -> Box.Box
lineS = Box.text

renderBox :: Box.Box -> String
renderBox = unlines
            . map (dropWhileEnd isSpace)
            . dropWhile whiteSpace
            . dropWhileEnd whiteSpace
            . lines
            . Box.render
  where
  whiteSpace = all isSpace

toTypelevelString :: Type a -> Maybe Box.Box
toTypelevelString (TypeLevelString _ s) =
  Just . Box.text $ decodeStringWithReplacement s
toTypelevelString (TypeApp _ (TypeConstructor _ f) x)
  | f == primSubName C.typeError "Text" = toTypelevelString x
toTypelevelString (TypeApp _ (KindApp _ (TypeConstructor _ f) _) x)
  | f == primSubName C.typeError "Quote" = Just (typeAsBox maxBound x)
toTypelevelString (TypeApp _ (TypeConstructor _ f) (TypeLevelString _ x))
  | f == primSubName C.typeError "QuoteLabel" = Just . line . prettyPrintLabel . Label $ x
toTypelevelString (TypeApp _ (TypeApp _ (TypeConstructor _ f) x) ret)
  | f == primSubName C.typeError "Beside" =
    (Box.<>) <$> toTypelevelString x <*> toTypelevelString ret
toTypelevelString (TypeApp _ (TypeApp _ (TypeConstructor _ f) x) ret)
  | f == primSubName C.typeError "Above" =
    (Box.//) <$> toTypelevelString x <*> toTypelevelString ret
toTypelevelString _ = Nothing

-- | Rethrow an error with a more detailed error message in the case of failure
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError (throwError . f)

warnAndRethrow :: (MonadError e m, MonadWriter e m) => (e -> e) -> m a -> m a
warnAndRethrow f = rethrow f . censor f

-- | Rethrow an error with source position information
rethrowWithPosition :: (MonadError MultipleErrors m) => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (onErrorMessages (withPosition pos))

warnWithPosition :: (MonadWriter MultipleErrors m) => SourceSpan -> m a -> m a
warnWithPosition pos = censor (onErrorMessages (withPosition pos))

warnAndRethrowWithPosition :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => SourceSpan -> m a -> m a
warnAndRethrowWithPosition pos = rethrowWithPosition pos . warnWithPosition pos

withPosition :: SourceSpan -> ErrorMessage -> ErrorMessage
withPosition NullSourceSpan err = err
withPosition pos (ErrorMessage hints se) = ErrorMessage (positionedError pos : hints) se

withoutPosition :: ErrorMessage -> ErrorMessage
withoutPosition (ErrorMessage hints se) = ErrorMessage (filter go hints) se
  where
  go (PositionedError _) = False
  go _ = True

positionedError :: SourceSpan -> ErrorMessageHint
positionedError = PositionedError . pure

-- | Runs a computation listening for warnings and then escalating any warnings
-- that match the predicate to error status.
escalateWarningWhen
  :: (MonadWriter MultipleErrors m, MonadError MultipleErrors m)
  => (ErrorMessage -> Bool)
  -> m a
  -> m a
escalateWarningWhen isError ma = do
  (a, w) <- censor (const mempty) $ listen ma
  let (errors, warnings) = partition isError (runMultipleErrors w)
  tell $ MultipleErrors warnings
  unless (null errors) $ throwError $ MultipleErrors errors
  return a

-- | Collect errors in in parallel
parU
  :: forall m a b
   . MonadError MultipleErrors m
  => [a]
  -> (a -> m b)
  -> m [b]
parU xs f =
    forM xs (withError . f) >>= collectErrors
  where
    withError :: m b -> m (Either MultipleErrors b)
    withError u = catchError (Right <$> u) (return . Left)

    collectErrors :: [Either MultipleErrors b] -> m [b]
    collectErrors es = case partitionEithers es of
      ([], rs) -> return rs
      (errs, _) -> throwError $ fold errs

internalCompilerError
  :: (MonadError MultipleErrors m, GHC.Stack.HasCallStack)
  => Text
  -> m a
internalCompilerError =
  throwError
    . errorMessage
    . InternalCompilerError (T.pack (GHC.Stack.prettyCallStack GHC.Stack.callStack))
