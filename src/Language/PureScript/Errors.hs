{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.PureScript.Errors where

import Prelude ()
import Prelude.Compat

import Data.Ord (comparing)
import Data.Char (isSpace)
import Data.Either (lefts, rights)
import Data.List (intercalate, transpose, nub, nubBy, sortBy, partition)
import Data.Foldable (fold)
import Data.Maybe (maybeToList)

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.State.Lazy
import Control.Arrow ((&&&))

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Pretty
import Language.PureScript.Pretty.Common (before)
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import qualified Language.PureScript.Bundle as Bundle

import qualified Text.PrettyPrint.Boxes as Box

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE
import Text.Parsec.Error (Message(..))

-- | A type of error messages
data SimpleErrorMessage
  = ErrorParsingFFIModule FilePath (Maybe Bundle.ErrorMessage)
  | ErrorParsingModule P.ParseError
  | MissingFFIModule ModuleName
  | MultipleFFIModules ModuleName [FilePath]
  | UnnecessaryFFIModule ModuleName FilePath
  | MissingFFIImplementations ModuleName [Ident]
  | UnusedFFIImplementations ModuleName [Ident]
  | InvalidFFIIdentifier ModuleName String
  | CannotGetFileInfo FilePath
  | CannotReadFile FilePath
  | CannotWriteFile FilePath
  | InfiniteType Type
  | InfiniteKind Kind
  | MultipleFixities Ident
  | OrphanTypeDeclaration Ident
  | OrphanFixityDeclaration String
  | RedefinedModule ModuleName [SourceSpan]
  | RedefinedIdent Ident
  | OverlappingNamesInLet
  | UnknownModule ModuleName
  | UnknownType (Qualified (ProperName 'TypeName))
  | UnknownTypeOp (Qualified Ident)
  | UnknownTypeClass (Qualified (ProperName 'ClassName))
  | UnknownValue (Qualified Ident)
  | UnknownDataConstructor (Qualified (ProperName 'ConstructorName)) (Maybe (Qualified (ProperName 'ConstructorName)))
  | UnknownTypeConstructor (Qualified (ProperName 'TypeName))
  | UnknownImportType ModuleName (ProperName 'TypeName)
  | UnknownExportType (ProperName 'TypeName)
  | UnknownImportTypeOp ModuleName Ident
  | UnknownExportTypeOp Ident
  | UnknownImportTypeClass ModuleName (ProperName 'ClassName)
  | UnknownExportTypeClass (ProperName 'ClassName)
  | UnknownImportValue ModuleName Ident
  | UnknownExportValue Ident
  | UnknownExportModule ModuleName
  | UnknownImportDataConstructor ModuleName (ProperName 'TypeName) (ProperName 'ConstructorName)
  | UnknownExportDataConstructor (ProperName 'TypeName) (ProperName 'ConstructorName)
  | ScopeConflict String [ModuleName]
  | ScopeShadowing String (Maybe ModuleName) [ModuleName]
  | ConflictingTypeDecls (ProperName 'TypeName)
  | ConflictingCtorDecls (ProperName 'ConstructorName)
  | TypeConflictsWithClass (ProperName 'TypeName)
  | CtorConflictsWithClass (ProperName 'ConstructorName)
  | ClassConflictsWithType (ProperName 'ClassName)
  | ClassConflictsWithCtor (ProperName 'ClassName)
  | DuplicateModuleName ModuleName
  | DuplicateClassExport (ProperName 'ClassName)
  | DuplicateValueExport Ident
  | DuplicateTypeOpExport Ident
  | DuplicateTypeArgument String
  | InvalidDoBind
  | InvalidDoLet
  | CycleInDeclaration Ident
  | CycleInTypeSynonym (Maybe (ProperName 'TypeName))
  | CycleInModules [ModuleName]
  | NameIsUndefined Ident
  | UndefinedTypeVariable (ProperName 'TypeName)
  | PartiallyAppliedSynonym (Qualified (ProperName 'TypeName))
  | EscapedSkolem (Maybe Expr)
  | TypesDoNotUnify Type Type
  | KindsDoNotUnify Kind Kind
  | ConstrainedTypeUnified Type Type
  | OverlappingInstances (Qualified (ProperName 'ClassName)) [Type] [Qualified Ident]
  | NoInstanceFound (Qualified (ProperName 'ClassName)) [Type]
  | PossiblyInfiniteInstance (Qualified (ProperName 'ClassName)) [Type]
  | CannotDerive (Qualified (ProperName 'ClassName)) [Type]
  | CannotFindDerivingType (ProperName 'TypeName)
  | DuplicateLabel String (Maybe Expr)
  | DuplicateValueDeclaration Ident
  | ArgListLengthsDiffer Ident
  | OverlappingArgNames (Maybe Ident)
  | MissingClassMember Ident
  | ExtraneousClassMember Ident (Qualified (ProperName 'ClassName))
  | ExpectedType Type Kind
  | IncorrectConstructorArity (Qualified (ProperName 'ConstructorName))
  | ExprDoesNotHaveType Expr Type
  | PropertyIsMissing String
  | AdditionalProperty String
  | CannotApplyFunction Type Expr
  | TypeSynonymInstance
  | OrphanInstance Ident (Qualified (ProperName 'ClassName)) [Type]
  | InvalidNewtype (ProperName 'TypeName)
  | InvalidInstanceHead Type
  | TransitiveExportError DeclarationRef [DeclarationRef]
  | TransitiveDctorExportError DeclarationRef (ProperName 'ConstructorName)
  | ShadowedName Ident
  | ShadowedTypeVar String
  | UnusedTypeVar String
  | WildcardInferredType Type
  | HoleInferredType String Type
  | MissingTypeDeclaration Ident Type
  | NotExhaustivePattern [[Binder]] Bool
  | OverlappingPattern [[Binder]] Bool
  | IncompleteExhaustivityCheck
  | ClassOperator (ProperName 'ClassName) Ident
  | MisleadingEmptyTypeImport ModuleName (ProperName 'TypeName)
  | ImportHidingModule ModuleName
  | UnusedImport ModuleName
  | UnusedExplicitImport ModuleName [String] (Maybe ModuleName) [DeclarationRef]
  | UnusedDctorImport (ProperName 'TypeName)
  | UnusedDctorExplicitImport (ProperName 'TypeName) [ProperName 'ConstructorName]
  | DeprecatedOperatorDecl String
  | DeprecatedOperatorSection Expr (Either Expr Expr)
  | DeprecatedQualifiedSyntax ModuleName ModuleName
  | DeprecatedClassImport ModuleName (ProperName 'ClassName)
  | DeprecatedClassExport (ProperName 'ClassName)
  | DuplicateSelectiveImport ModuleName
  | DuplicateImport ModuleName ImportDeclarationType (Maybe ModuleName)
  | DuplicateImportRef String
  | DuplicateExportRef String
  | IntOutOfRange Integer String Integer Integer
  | RedundantEmptyHidingImport ModuleName
  | ImplicitQualifiedImport ModuleName ModuleName [DeclarationRef]
  | ImplicitImport ModuleName [DeclarationRef]
  | HidingImport ModuleName [DeclarationRef]
  | CaseBinderLengthDiffers Int [Binder]
  | IncorrectAnonymousArgument
  | InvalidOperatorInBinder Ident Ident
  | DeprecatedRequirePath
  | CannotGeneralizeRecursiveFunction Ident Type
  deriving (Show)

-- | Error message hints, providing more detailed information about failure.
data ErrorMessageHint
  = ErrorUnifyingTypes Type Type
  | ErrorInExpression Expr
  | ErrorInModule ModuleName
  | ErrorInInstance (Qualified (ProperName 'ClassName)) [Type]
  | ErrorInSubsumption Type Type
  | ErrorCheckingAccessor Expr String
  | ErrorCheckingType Expr Type
  | ErrorCheckingKind Type
  | ErrorCheckingGuard
  | ErrorInferringType Expr
  | ErrorInApplication Expr Type Expr
  | ErrorInDataConstructor (ProperName 'ConstructorName)
  | ErrorInTypeConstructor (ProperName 'TypeName)
  | ErrorInBindingGroup [Ident]
  | ErrorInDataBindingGroup
  | ErrorInTypeSynonym (ProperName 'TypeName)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | ErrorInForeignImport Ident
  | PositionedError SourceSpan
  deriving Show

-- | Categories of hints
data HintCategory
  = ExprHint
  | KindHint
  | CheckHint
  | PositionHint
  | OtherHint
  deriving (Show, Eq)

data ErrorMessage = ErrorMessage [ErrorMessageHint] SimpleErrorMessage deriving (Show)

newtype ErrorSuggestion = ErrorSuggestion String

-- | Get the source span for an error
errorSpan :: ErrorMessage -> Maybe SourceSpan
errorSpan = findHint matchSpan
  where
  matchSpan (PositionedError ss) = Just ss
  matchSpan _ = Nothing

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

-- |
-- Get the error code for a particular error type
--
errorCode :: ErrorMessage -> String
errorCode em = case unwrapErrorMessage em of
  ErrorParsingFFIModule{} -> "ErrorParsingFFIModule"
  ErrorParsingModule{} -> "ErrorParsingModule"
  MissingFFIModule{} -> "MissingFFIModule"
  MultipleFFIModules{} -> "MultipleFFIModules"
  UnnecessaryFFIModule{} -> "UnnecessaryFFIModule"
  MissingFFIImplementations{} -> "MissingFFIImplementations"
  UnusedFFIImplementations{} -> "UnusedFFIImplementations"
  InvalidFFIIdentifier{} -> "InvalidFFIIdentifier"
  CannotGetFileInfo{} -> "CannotGetFileInfo"
  CannotReadFile{} -> "CannotReadFile"
  CannotWriteFile{} -> "CannotWriteFile"
  InfiniteType{} -> "InfiniteType"
  InfiniteKind{} -> "InfiniteKind"
  MultipleFixities{} -> "MultipleFixities"
  OrphanTypeDeclaration{} -> "OrphanTypeDeclaration"
  OrphanFixityDeclaration{} -> "OrphanFixityDeclaration"
  RedefinedModule{} -> "RedefinedModule"
  RedefinedIdent{} -> "RedefinedIdent"
  OverlappingNamesInLet -> "OverlappingNamesInLet"
  UnknownModule{} -> "UnknownModule"
  UnknownType{} -> "UnknownType"
  UnknownTypeOp{} -> "UnknownTypeOp"
  UnknownTypeClass{} -> "UnknownTypeClass"
  UnknownValue{} -> "UnknownValue"
  UnknownDataConstructor{} -> "UnknownDataConstructor"
  UnknownTypeConstructor{} -> "UnknownTypeConstructor"
  UnknownImportType{} -> "UnknownImportType"
  UnknownImportTypeOp{} -> "UnknownImportTypeOp"
  UnknownExportType{} -> "UnknownExportType"
  UnknownExportTypeOp{} -> "UnknownExportTypeOp"
  UnknownImportTypeClass{} -> "UnknownImportTypeClass"
  UnknownExportTypeClass{} -> "UnknownExportTypeClass"
  UnknownImportValue{} -> "UnknownImportValue"
  UnknownExportValue{} -> "UnknownExportValue"
  UnknownExportModule{} -> "UnknownExportModule"
  UnknownImportDataConstructor{} -> "UnknownImportDataConstructor"
  UnknownExportDataConstructor{} -> "UnknownExportDataConstructor"
  ScopeConflict{} -> "ScopeConflict"
  ScopeShadowing{} -> "ScopeShadowing"
  ConflictingTypeDecls{} -> "ConflictingTypeDecls"
  ConflictingCtorDecls{} -> "ConflictingCtorDecls"
  TypeConflictsWithClass{} -> "TypeConflictsWithClass"
  CtorConflictsWithClass{} -> "CtorConflictsWithClass"
  ClassConflictsWithType{} -> "ClassConflictsWithType"
  ClassConflictsWithCtor{} -> "ClassConflictsWithCtor"
  DuplicateModuleName{} -> "DuplicateModuleName"
  DuplicateClassExport{} -> "DuplicateClassExport"
  DuplicateValueExport{} -> "DuplicateValueExport"
  DuplicateTypeOpExport{} -> "DuplicateTypeOpExport"
  DuplicateTypeArgument{} -> "DuplicateTypeArgument"
  InvalidDoBind -> "InvalidDoBind"
  InvalidDoLet -> "InvalidDoLet"
  CycleInDeclaration{} -> "CycleInDeclaration"
  CycleInTypeSynonym{} -> "CycleInTypeSynonym"
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
  PossiblyInfiniteInstance{} -> "PossiblyInfiniteInstance"
  CannotDerive{} -> "CannotDerive"
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
  CannotApplyFunction{} -> "CannotApplyFunction"
  TypeSynonymInstance -> "TypeSynonymInstance"
  OrphanInstance{} -> "OrphanInstance"
  InvalidNewtype{} -> "InvalidNewtype"
  InvalidInstanceHead{} -> "InvalidInstanceHead"
  TransitiveExportError{} -> "TransitiveExportError"
  TransitiveDctorExportError{} -> "TransitiveDctorExportError"
  ShadowedName{} -> "ShadowedName"
  ShadowedTypeVar{} -> "ShadowedTypeVar"
  UnusedTypeVar{} -> "UnusedTypeVar"
  WildcardInferredType{} -> "WildcardInferredType"
  HoleInferredType{} -> "HoleInferredType"
  MissingTypeDeclaration{} -> "MissingTypeDeclaration"
  NotExhaustivePattern{} -> "NotExhaustivePattern"
  OverlappingPattern{} -> "OverlappingPattern"
  IncompleteExhaustivityCheck{} -> "IncompleteExhaustivityCheck"
  ClassOperator{} -> "ClassOperator"
  MisleadingEmptyTypeImport{} -> "MisleadingEmptyTypeImport"
  ImportHidingModule{} -> "ImportHidingModule"
  UnusedImport{} -> "UnusedImport"
  UnusedExplicitImport{} -> "UnusedExplicitImport"
  UnusedDctorImport{} -> "UnusedDctorImport"
  UnusedDctorExplicitImport{} -> "UnusedDctorExplicitImport"
  DeprecatedOperatorDecl{} -> "DeprecatedOperatorDecl"
  DeprecatedOperatorSection{} -> "DeprecatedOperatorSection"
  DeprecatedQualifiedSyntax{} -> "DeprecatedQualifiedSyntax"
  DeprecatedClassImport{} -> "DeprecatedClassImport"
  DeprecatedClassExport{} -> "DeprecatedClassExport"
  DuplicateSelectiveImport{} -> "DuplicateSelectiveImport"
  DuplicateImport{} -> "DuplicateImport"
  DuplicateImportRef{} -> "DuplicateImportRef"
  DuplicateExportRef{} -> "DuplicateExportRef"
  IntOutOfRange{} -> "IntOutOfRange"
  RedundantEmptyHidingImport{} -> "RedundantEmptyHidingImport"
  ImplicitQualifiedImport{} -> "ImplicitQualifiedImport"
  ImplicitImport{} -> "ImplicitImport"
  HidingImport{} -> "HidingImport"
  CaseBinderLengthDiffers{} -> "CaseBinderLengthDiffers"
  IncorrectAnonymousArgument -> "IncorrectAnonymousArgument"
  InvalidOperatorInBinder{} -> "InvalidOperatorInBinder"
  DeprecatedRequirePath{} -> "DeprecatedRequirePath"
  CannotGeneralizeRecursiveFunction{} -> "CannotGeneralizeRecursiveFunction"

-- |
-- A stack trace for an error
--
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage] } deriving (Show, Monoid)

-- | Check whether a collection of errors is empty or not.
nonEmpty :: MultipleErrors -> Bool
nonEmpty = not . null . runMultipleErrors

-- |
-- Create an error set from a single simple error message
--
errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [ErrorMessage [] err]

-- |
-- Create an error set from a single error message
--
singleError :: ErrorMessage -> MultipleErrors
singleError = MultipleErrors . pure

-- | Lift a function on ErrorMessage to a function on MultipleErrors
onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f = MultipleErrors . map f . runMultipleErrors

-- | Add a hint to an error message
addHint :: ErrorMessageHint -> MultipleErrors -> MultipleErrors
addHint hint = onErrorMessages $ \(ErrorMessage hints se) -> ErrorMessage (hint : hints) se

-- | A map from rigid type variable name/unknown variable pairs to new variables.
data TypeMap = TypeMap
  { umSkolemMap :: M.Map Int (String, Int, Maybe SourceSpan)
  , umNextSkolem :: Int
  , umUnknownMap :: M.Map Int Int
  , umNextUnknown :: Int
  } deriving Show

defaultUnknownMap :: TypeMap
defaultUnknownMap = TypeMap M.empty 0 M.empty 0

-- | How critical the issue is
data Level = Error | Warning deriving Show

-- |
-- Extract nested error messages from wrapper errors
--
unwrapErrorMessage :: ErrorMessage -> SimpleErrorMessage
unwrapErrorMessage (ErrorMessage _ se) = se

replaceUnknowns :: Type -> State TypeMap Type
replaceUnknowns = everywhereOnTypesM replaceTypes
  where
  replaceTypes :: Type -> State TypeMap Type
  replaceTypes (TUnknown u) = do
    m <- get
    case M.lookup u (umUnknownMap m) of
      Nothing -> do
        let u' = umNextUnknown m
        put $ m { umUnknownMap = M.insert u u' (umUnknownMap m), umNextUnknown = u' + 1 }
        return (TUnknown u')
      Just u' -> return (TUnknown u')
  replaceTypes (Skolem name s sko ss) = do
    m <- get
    case M.lookup s (umSkolemMap m) of
      Nothing -> do
        let s' = umNextSkolem m
        put $ m { umSkolemMap = M.insert s (name, s', ss) (umSkolemMap m), umNextSkolem = s' + 1 }
        return (Skolem name s' sko ss)
      Just (_, s', _) -> return (Skolem name s' sko ss)
  replaceTypes other = return other

onTypesInErrorMessageM :: (Applicative m) => (Type -> m Type) -> ErrorMessage -> m ErrorMessage
onTypesInErrorMessageM f (ErrorMessage hints simple) = ErrorMessage <$> traverse gHint hints <*> gSimple simple
  where
  gSimple (InfiniteType t) = InfiniteType <$> f t
  gSimple (TypesDoNotUnify t1 t2) = TypesDoNotUnify <$> f t1 <*> f t2
  gSimple (ConstrainedTypeUnified t1 t2) = ConstrainedTypeUnified <$> f t1 <*> f t2
  gSimple (ExprDoesNotHaveType e t) = ExprDoesNotHaveType e <$> f t
  gSimple (CannotApplyFunction t e) = CannotApplyFunction <$> f t <*> pure e
  gSimple (InvalidInstanceHead t) = InvalidInstanceHead <$> f t
  gSimple (NoInstanceFound cl ts) = NoInstanceFound cl <$> traverse f ts
  gSimple (OverlappingInstances cl ts insts) = OverlappingInstances cl <$> traverse f ts <*> pure insts
  gSimple (PossiblyInfiniteInstance cl ts) = PossiblyInfiniteInstance cl <$> traverse f ts
  gSimple (CannotDerive cl ts) = CannotDerive cl <$> traverse f ts
  gSimple (ExpectedType ty k) = ExpectedType <$> f ty <*> pure k
  gSimple (OrphanInstance nm cl ts) = OrphanInstance nm cl <$> traverse f ts
  gSimple (WildcardInferredType ty) = WildcardInferredType <$> f ty
  gSimple (HoleInferredType name ty) = HoleInferredType name <$> f ty
  gSimple (MissingTypeDeclaration nm ty) = MissingTypeDeclaration nm <$> f ty
  gSimple (CannotGeneralizeRecursiveFunction nm ty) = CannotGeneralizeRecursiveFunction nm <$> f ty

  gSimple other = pure other

  gHint (ErrorInSubsumption t1 t2) = ErrorInSubsumption <$> f t1 <*> f t2
  gHint (ErrorUnifyingTypes t1 t2) = ErrorUnifyingTypes <$> f t1 <*> f t2
  gHint (ErrorCheckingType e t) = ErrorCheckingType e <$> f t
  gHint (ErrorCheckingKind t) = ErrorCheckingKind <$> f t
  gHint (ErrorInApplication e1 t1 e2) = ErrorInApplication e1 <$> f t1 <*> pure e2
  gHint (ErrorInInstance cl ts) = ErrorInInstance cl <$> traverse f ts
  gHint other = pure other

wikiUri :: ErrorMessage -> String
wikiUri e = "https://github.com/purescript/purescript/wiki/Error-Code-" ++ errorCode e

-- TODO Other possible suggestions:
-- WildcardInferredType - source span not small enough
-- DuplicateSelectiveImport - would require 2 ranges to remove and 1 insert
-- DeprecatedClassExport, DeprecatedClassImport, DeprecatedOperatorSection, would want to replace smaller span?
errorSuggestion :: SimpleErrorMessage -> Maybe ErrorSuggestion
errorSuggestion err = case err of
  UnusedImport{} -> emptySuggestion
  RedundantEmptyHidingImport{} -> emptySuggestion
  DuplicateImport{} -> emptySuggestion
  DeprecatedQualifiedSyntax name qualName -> suggest $
    "import " ++ runModuleName name ++ " as " ++ runModuleName qualName
  UnusedExplicitImport mn _ qual refs -> suggest $ importSuggestion mn refs qual
  ImplicitImport mn refs -> suggest $ importSuggestion mn refs Nothing
  ImplicitQualifiedImport mn asModule refs -> suggest $ importSuggestion mn refs (Just asModule)
  HidingImport mn refs -> suggest $ importSuggestion mn refs Nothing
  _ -> Nothing

  where
    emptySuggestion = Just $ ErrorSuggestion ""
    suggest = Just . ErrorSuggestion

    importSuggestion :: ModuleName -> [ DeclarationRef ] -> Maybe ModuleName -> String
    importSuggestion mn refs qual =
      "import " ++ runModuleName mn ++ " (" ++ intercalate ", " (map prettyPrintRef refs) ++ ")" ++ qstr qual

    qstr :: Maybe ModuleName -> String
    qstr (Just mn) = " as " ++ runModuleName mn
    qstr Nothing = ""

showSuggestion :: SimpleErrorMessage -> String
showSuggestion suggestion = case errorSuggestion suggestion of
  Just (ErrorSuggestion x) -> x
  _ -> ""

-- |
-- Pretty print a single error, simplifying if necessary
--
prettyPrintSingleError :: Bool -> Level -> Bool -> ErrorMessage -> Box.Box
prettyPrintSingleError full level showWiki e = flip evalState defaultUnknownMap $ do
  em <- onTypesInErrorMessageM replaceUnknowns (if full then e else simplifyErrorMessage e)
  um <- get
  return (prettyPrintErrorMessage um em)
  where

  -- Pretty print an ErrorMessage
  prettyPrintErrorMessage :: TypeMap -> ErrorMessage -> Box.Box
  prettyPrintErrorMessage typeMap (ErrorMessage hints simple) =
    paras $
      [ foldr renderHint (indent (renderSimpleErrorMessage simple)) hints
      ] ++
      maybe [] (return . Box.moveDown 1) typeInformation ++
      [ Box.moveDown 1 $ paras [ line $ "See " ++ wikiUri e ++ " for more information, "
                                 , line $ "or to contribute content related to this " ++ levelText ++ "."
                                 ]
      | showWiki
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
          line (name ++ show s ++ " is a rigid type variable")
          : foldMap (return . line . ("  bound at " ++) . displayStartEndPos) ss

      unknownInfo :: Int -> Box.Box
      unknownInfo u = line $ "_" ++ show u ++ " is an unknown type"

    renderSimpleErrorMessage :: SimpleErrorMessage -> Box.Box
    renderSimpleErrorMessage (CannotGetFileInfo path) =
      paras [ line "Unable to read file info: "
            , indent . line $ path
            ]
    renderSimpleErrorMessage (CannotReadFile path) =
      paras [ line "Unable to read file: "
            , indent . line $ path
            ]
    renderSimpleErrorMessage (CannotWriteFile path) =
      paras [ line "Unable to write file: "
            , indent . line $ path
            ]
    renderSimpleErrorMessage (ErrorParsingFFIModule path extra) =
      paras $ [ line "Unable to parse foreign module:"
              , indent . line $ path
              ] ++
              (map (indent . line) (concatMap Bundle.printErrorMessage (maybeToList extra)))
    renderSimpleErrorMessage (ErrorParsingModule err) =
      paras [ line "Unable to parse module: "
            , prettyPrintParseError err
            ]
    renderSimpleErrorMessage (MissingFFIModule mn) =
      line $ "The foreign module implementation for module " ++ runModuleName mn ++ " is missing."
    renderSimpleErrorMessage (UnnecessaryFFIModule mn path) =
      paras [ line $ "An unnecessary foreign module implementation was provided for module " ++ runModuleName mn ++ ": "
            , indent . line $ path
            , line $ "Module " ++ runModuleName mn ++ " does not contain any foreign import declarations, so a foreign module is not necessary."
            ]
    renderSimpleErrorMessage (MissingFFIImplementations mn idents) =
      paras [ line $ "The following values are not defined in the foreign module for module " ++ runModuleName mn ++ ": "
            , indent . paras $ map (line . runIdent) idents
            ]
    renderSimpleErrorMessage (UnusedFFIImplementations mn idents) =
      paras [ line $ "The following definitions in the foreign module for module " ++ runModuleName mn ++ " are unused: "
            , indent . paras $ map (line . runIdent) idents
            ]
    renderSimpleErrorMessage (InvalidFFIIdentifier mn ident) =
      paras [ line $ "In the FFI module for " ++ runModuleName mn ++ ":"
            , indent . paras $
                [ line $ "The identifier `" ++ ident ++ "` is not valid in PureScript."
                , line "Note that exported identifiers in FFI modules must be valid PureScript identifiers."
                ]
            ]
    renderSimpleErrorMessage (MultipleFFIModules mn paths) =
      paras [ line $ "Multiple foreign module implementations have been provided for module " ++ runModuleName mn ++ ": "
            , indent . paras $ map line paths
            ]
    renderSimpleErrorMessage InvalidDoBind =
      line "The last statement in a 'do' block must be an expression, but this block ends with a binder."
    renderSimpleErrorMessage InvalidDoLet =
      line "The last statement in a 'do' block must be an expression, but this block ends with a let binding."
    renderSimpleErrorMessage OverlappingNamesInLet =
      line "The same name was used more than once in a let binding."
    renderSimpleErrorMessage (InfiniteType ty) =
      paras [ line "An infinite type was inferred for an expression: "
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (InfiniteKind ki) =
      paras [ line "An infinite kind was inferred for a type: "
            , indent $ line $ prettyPrintKind ki
            ]
    renderSimpleErrorMessage (MultipleFixities name) =
      line $ "There are multiple fixity/precedence declarations for " ++ showIdent name
    renderSimpleErrorMessage (OrphanTypeDeclaration nm) =
      line $ "The type declaration for " ++ showIdent nm ++ " should be followed by its definition."
    renderSimpleErrorMessage (OrphanFixityDeclaration op) =
      line $ "The fixity/precedence declaration for " ++ show op ++ " should appear in the same module as its definition."
    renderSimpleErrorMessage (RedefinedModule name filenames) =
      paras [ line ("The module " ++ runModuleName name ++ " has been defined multiple times:")
            , indent . paras $ map (line . displaySourceSpan) filenames
            ]
    renderSimpleErrorMessage (RedefinedIdent name) =
      line $ "The value " ++ showIdent name ++ " has been defined multiple times"
    renderSimpleErrorMessage (UnknownModule mn) =
      line $ "Unknown module " ++ runModuleName mn
    renderSimpleErrorMessage (UnknownType name) =
      line $ "Unknown type " ++ showQualified runProperName name
    renderSimpleErrorMessage (UnknownTypeOp name) =
      line $ "Unknown type operator " ++ showQualified showIdent name
    renderSimpleErrorMessage (UnknownTypeClass name) =
      line $ "Unknown type class " ++ showQualified runProperName name
    renderSimpleErrorMessage (UnknownValue name) =
      line $ "Unknown value " ++ showQualified showIdent name
    renderSimpleErrorMessage (UnknownTypeConstructor name) =
      line $ "Unknown type constructor " ++ showQualified runProperName name
    renderSimpleErrorMessage (UnknownDataConstructor dc tc) =
      line $ "Unknown data constructor " ++ showQualified runProperName dc ++ foldMap ((" for type constructor " ++) . showQualified runProperName) tc
    renderSimpleErrorMessage (UnknownImportType mn name) =
      paras [ line $ "Cannot import type " ++ runProperName name ++ " from module " ++ runModuleName mn
            , line "It either does not exist or the module does not export it."
            ]
    renderSimpleErrorMessage (UnknownExportType name) =
      line $ "Cannot export unknown type " ++ runProperName name
    renderSimpleErrorMessage (UnknownImportTypeOp mn name) =
      paras [ line $ "Cannot import type operator " ++ showIdent name ++ " from module " ++ runModuleName mn
            , line "It either does not exist or the module does not export it."
            ]
    renderSimpleErrorMessage (UnknownExportTypeOp name) =
      line $ "Cannot export unknown type operator " ++ showIdent name
    renderSimpleErrorMessage (UnknownImportTypeClass mn name) =
      paras [ line $ "Cannot import type class " ++ runProperName name ++ " from module " ++ runModuleName mn
            , line "It either does not exist or the module does not export it."
            ]
    renderSimpleErrorMessage (UnknownExportTypeClass name) =
      line $ "Cannot export unknown type class " ++ runProperName name
    renderSimpleErrorMessage (UnknownImportValue mn name) =
      paras [ line $ "Cannot import value " ++ showIdent name ++ " from module " ++ runModuleName mn
            , line "It either does not exist or the module does not export it."
            ]
    renderSimpleErrorMessage (UnknownExportValue name) =
      line $ "Cannot export unknown value " ++ showIdent name
    renderSimpleErrorMessage (UnknownExportModule name) =
      paras [ line $ "Cannot export unknown module " ++ runModuleName name
            , line "It either does not exist or has not been imported by the current module."
            ]
    renderSimpleErrorMessage (UnknownImportDataConstructor mn tcon dcon) =
      line $ "Module " ++ runModuleName mn ++ " does not export data constructor " ++ runProperName dcon ++ " for type " ++ runProperName tcon
    renderSimpleErrorMessage (UnknownExportDataConstructor tcon dcon) =
      line $ "Cannot export data constructor " ++ runProperName dcon ++ " for type " ++ runProperName tcon ++ ", as it has not been declared."
    renderSimpleErrorMessage (ScopeConflict nm ms) =
      paras [ line $ "Conflicting definitions are in scope for " ++ nm ++ " from the following modules:"
            , indent $ paras $ map (line . runModuleName) ms
            ]
    renderSimpleErrorMessage (ScopeShadowing nm exmn ms) =
      paras [ line $ "Shadowed definitions are in scope for " ++ nm ++ " from the following open imports:"
            , indent $ paras $ map (line . ("import " ++) . runModuleName) ms
            , line $ "These will be ignored and the " ++ case exmn of
                Just exmn' -> "declaration from " ++ runModuleName exmn' ++ " will be used."
                Nothing -> "local declaration will be used."
            ]
    renderSimpleErrorMessage (ConflictingTypeDecls nm) =
      line $ "Conflicting type declarations for " ++ runProperName nm
    renderSimpleErrorMessage (ConflictingCtorDecls nm) =
      line $ "Conflicting data constructor declarations for " ++ runProperName nm
    renderSimpleErrorMessage (TypeConflictsWithClass nm) =
      line $ "Type " ++ runProperName nm ++ " conflicts with a type class declaration with the same name."
    renderSimpleErrorMessage (CtorConflictsWithClass nm) =
      line $ "Data constructor " ++ runProperName nm ++ " conflicts with a type class declaration with the same name."
    renderSimpleErrorMessage (ClassConflictsWithType nm) =
      line $ "Type class " ++ runProperName nm ++ " conflicts with a type declaration with the same name."
    renderSimpleErrorMessage (ClassConflictsWithCtor nm) =
      line $ "Type class " ++ runProperName nm ++ " conflicts with a data constructor declaration with the same name."
    renderSimpleErrorMessage (DuplicateModuleName mn) =
      line $ "Module " ++ runModuleName mn ++ " has been defined multiple times."
    renderSimpleErrorMessage (DuplicateClassExport nm) =
      line $ "Duplicate export declaration for type class " ++ runProperName nm
    renderSimpleErrorMessage (DuplicateValueExport nm) =
      line $ "Duplicate export declaration for value " ++ showIdent nm
    renderSimpleErrorMessage (DuplicateTypeOpExport nm) =
      line $ "Duplicate export declaration for type operator " ++ showIdent nm
    renderSimpleErrorMessage (CycleInDeclaration nm) =
      line $ "The value of " ++ showIdent nm ++ " is undefined here, so this reference is not allowed."
    renderSimpleErrorMessage (CycleInModules mns) =
      paras [ line "There is a cycle in module dependencies in these modules: "
            , indent $ paras (map (line . runModuleName) mns)
            ]
    renderSimpleErrorMessage (CycleInTypeSynonym name) =
      paras [ line $ case name of
                       Just pn -> "A cycle appears in the definition of type synonym " ++ runProperName pn
                       Nothing -> "A cycle appears in a set of type synonym definitions."
            , line "Cycles are disallowed because they can lead to loops in the type checker."
            , line "Consider using a 'newtype' instead."
            ]
    renderSimpleErrorMessage (NameIsUndefined ident) =
      line $ "Value " ++ showIdent ident ++ " is undefined."
    renderSimpleErrorMessage (UndefinedTypeVariable name) =
      line $ "Type variable " ++ runProperName name ++ " is undefined."
    renderSimpleErrorMessage (PartiallyAppliedSynonym name) =
      paras [ line $ "Type synonym " ++ showQualified runProperName name ++ " is partially applied."
            , line "Type synonyms must be applied to all of their type arguments."
            ]
    renderSimpleErrorMessage (EscapedSkolem binding) =
      paras $ [ line "A type variable has escaped its scope." ]
                     <> foldMap (\expr -> [ line "Relevant expression: "
                                          , indent $ prettyPrintValue valueDepth expr
                                          ]) binding
    renderSimpleErrorMessage (TypesDoNotUnify u1 u2)
      = let (sorted1, sorted2) = sortRows u1 u2

            sortRows :: Type -> Type -> (Type, Type)
            sortRows r1@RCons{} r2@RCons{} = sortRows' (rowToList r1) (rowToList r2)
            sortRows t1 t2 = (t1, t2)

            -- Put the common labels last
            sortRows' :: ([(String, Type)], Type) -> ([(String, Type)], Type) -> (Type, Type)
            sortRows' (s1, r1) (s2, r2) =
              let common :: [(String, (Type, Type))]
                  common = sortBy (comparing fst) [ (name, (t1, t2)) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]

                  sd1, sd2 :: [(String, Type)]
                  sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
                  sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
              in ( rowFromList (sortBy (comparing fst) sd1 ++ map (fst &&& fst . snd) common, r1)
                 , rowFromList (sortBy (comparing fst) sd2 ++ map (fst &&& snd . snd) common, r2)
                 )
        in paras [ line "Could not match type"
                 , indent $ typeAsBox sorted1
                 , line "with type"
                 , indent $ typeAsBox sorted2
                 ]

    renderSimpleErrorMessage (KindsDoNotUnify k1 k2) =
      paras [ line "Could not match kind"
            , indent $ line $ prettyPrintKind k1
            , line "with kind"
            , indent $ line $ prettyPrintKind k2
            ]
    renderSimpleErrorMessage (ConstrainedTypeUnified t1 t2) =
      paras [ line "Could not match constrained type"
            , indent $ typeAsBox t1
            , line "with type"
            , indent $ typeAsBox t2
            ]
    renderSimpleErrorMessage (OverlappingInstances nm ts (d : ds)) =
      paras [ line "Overlapping type class instances found for"
            , indent $ Box.hsep 1 Box.left [ line (showQualified runProperName nm)
                                           , Box.vcat Box.left (map typeAtomAsBox ts)
                                           ]
            , line "The following instances were found:"
            , indent $ paras (line (showQualified showIdent d ++ " (chosen)") : map (line . showQualified showIdent) ds)
            , line "Overlapping type class instances can lead to different behavior based on the order of module imports, and for that reason are not recommended."
            , line "They may be disallowed completely in a future version of the compiler."
            ]
    renderSimpleErrorMessage OverlappingInstances{} = internalError "OverlappingInstances: empty instance list"
    renderSimpleErrorMessage (NoInstanceFound nm ts) =
      paras [ line "No type class instance was found for"
            , indent $ Box.hsep 1 Box.left [ line (showQualified runProperName nm)
                                           , Box.vcat Box.left (map typeAtomAsBox ts)
                                           ]
            , paras [ line "The instance head contains unknown type variables. Consider adding a type annotation."
                    | any containsUnknowns ts
                    ]
            ]
      where
      containsUnknowns :: Type -> Bool
      containsUnknowns = everythingOnTypes (||) go
        where
        go TUnknown{} = True
        go _ = False
    renderSimpleErrorMessage (PossiblyInfiniteInstance nm ts) =
      paras [ line "Type class instance for"
            , indent $ Box.hsep 1 Box.left [ line (showQualified runProperName nm)
                                           , Box.vcat Box.left (map typeAtomAsBox ts)
                                           ]
            , line "is possibly infinite."
            ]
    renderSimpleErrorMessage (CannotDerive nm ts) =
      paras [ line "Cannot derive a type class instance for"
            , indent $ Box.hsep 1 Box.left [ line (showQualified runProperName nm)
                                           , Box.vcat Box.left (map typeAtomAsBox ts)
                                           ]
            ]
    renderSimpleErrorMessage (CannotFindDerivingType nm) =
      line $ "Cannot derive a type class instance, because the type declaration for " ++ runProperName nm ++ " could not be found."
    renderSimpleErrorMessage (DuplicateLabel l expr) =
      paras $ [ line $ "Label " ++ show l ++ " appears more than once in a row type." ]
                       <> foldMap (\expr' -> [ line "Relevant expression: "
                                             , indent $ prettyPrintValue valueDepth expr'
                                             ]) expr
    renderSimpleErrorMessage (DuplicateTypeArgument name) =
      line $ "Type argument " ++ show name ++ " appears more than once."
    renderSimpleErrorMessage (DuplicateValueDeclaration nm) =
      line $ "Multiple value declarations exist for " ++ showIdent nm ++ "."
    renderSimpleErrorMessage (ArgListLengthsDiffer ident) =
      line $ "Argument list lengths differ in declaration " ++ showIdent ident
    renderSimpleErrorMessage (OverlappingArgNames ident) =
      line $ "Overlapping names in function/binder" ++ foldMap ((" in declaration " ++) . showIdent) ident
    renderSimpleErrorMessage (MissingClassMember ident) =
      line $ "Type class member " ++ showIdent ident ++ " has not been implemented."
    renderSimpleErrorMessage (ExtraneousClassMember ident className) =
      line $ showIdent ident ++ " is not a member of type class " ++ showQualified runProperName className
    renderSimpleErrorMessage (ExpectedType ty kind) =
      paras [ line "In a type-annotated expression x :: t, the type t must have kind *."
            , line "The error arises from the type"
            , indent $ typeAsBox ty
            , line "having the kind"
            , indent $ line $ prettyPrintKind kind
            , line "instead."
            ]
    renderSimpleErrorMessage (IncorrectConstructorArity nm) =
      line $ "Data constructor " ++ showQualified runProperName nm ++ " was given the wrong number of arguments in a case expression."
    renderSimpleErrorMessage (ExprDoesNotHaveType expr ty) =
      paras [ line "Expression"
            , indent $ prettyPrintValue valueDepth expr
            , line "does not have type"
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (PropertyIsMissing prop) =
      line $ "Type of expression lacks required label " ++ show prop ++ "."
    renderSimpleErrorMessage (AdditionalProperty prop) =
      line $ "Type of expression contains additional label " ++ show prop ++ "."
    renderSimpleErrorMessage (CannotApplyFunction fn arg) =
      paras [ line "A function of type"
            , indent $ typeAsBox fn
            , line "can not be applied to the argument"
            , indent $ prettyPrintValue valueDepth arg
            ]
    renderSimpleErrorMessage TypeSynonymInstance =
      line "Type class instances for type synonyms are disallowed."
    renderSimpleErrorMessage (OrphanInstance nm cnm ts) =
      paras [ line $ "Type class instance " ++ showIdent nm ++ " for "
            , indent $ Box.hsep 1 Box.left [ line (showQualified runProperName cnm)
                                           , Box.vcat Box.left (map typeAtomAsBox ts)
                                           ]
            , line "is an orphan instance."
            , line "An orphan instance is an instance which is defined in neither the class module nor the data type module."
            , line "Consider moving the instance, if possible, or using a newtype wrapper."
            ]
    renderSimpleErrorMessage (InvalidNewtype name) =
      paras [ line $ "Newtype " ++ runProperName name ++ " is invalid."
            , line "Newtypes must define a single constructor with a single argument."
            ]
    renderSimpleErrorMessage (InvalidInstanceHead ty) =
      paras [ line "Type class instance head is invalid due to use of type"
            , indent $ typeAsBox ty
            , line "All types appearing in instance declarations must be of the form T a_1 .. a_n, where each type a_i is of the same form."
            ]
    renderSimpleErrorMessage (TransitiveExportError x ys) =
      paras [ line $ "An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: "
            , indent $ paras $ map (line . prettyPrintExport) ys
            ]
    renderSimpleErrorMessage (TransitiveDctorExportError x ctor) =
      paras [ line $ "An export for " ++ prettyPrintExport x ++ " requires the following data constructor to also be exported: "
            , indent $ line $ runProperName ctor
            ]
    renderSimpleErrorMessage (ShadowedName nm) =
      line $ "Name '" ++ showIdent nm ++ "' was shadowed."
    renderSimpleErrorMessage (ShadowedTypeVar tv) =
      line $ "Type variable '" ++ tv ++ "' was shadowed."
    renderSimpleErrorMessage (UnusedTypeVar tv) =
      line $ "Type variable '" ++ tv ++ "' was declared but not used."
    renderSimpleErrorMessage (ClassOperator className opName) =
      paras [ line $ "Type class '" ++ runProperName className ++ "' declares operator " ++ showIdent opName ++ "."
            , line "This may be disallowed in the future - consider declaring a named member in the class and making the operator an alias:"
            , indent . line $ showIdent opName ++ " = someMember"
            ]
    renderSimpleErrorMessage (MisleadingEmptyTypeImport mn name) =
      line $ "Importing type " ++ runProperName name ++ "(..) from " ++ runModuleName mn ++ " is misleading as it has no exported data constructors."
    renderSimpleErrorMessage (ImportHidingModule name) =
      paras [ line "'hiding' imports cannot be used to hide modules."
            , line $ "An attempt was made to hide the import of " ++ runModuleName name
            ]
    renderSimpleErrorMessage (WildcardInferredType ty) =
      paras [ line "Wildcard type definition has the inferred type "
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (HoleInferredType name ty) =
      paras [ line $ "Hole '" ++ name ++ "' has the inferred type "
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (MissingTypeDeclaration ident ty) =
      paras [ line $ "No type declaration was provided for the top-level declaration of " ++ showIdent ident ++ "."
            , line "It is good practice to provide type declarations as a form of documentation."
            , line $ "The inferred type of " ++ showIdent ident ++ " was:"
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (NotExhaustivePattern bs b) =
      paras [ line "A case expression could not be determined to cover all inputs."
            , line "The following additional cases are required to cover all inputs:\n"
            , indent $ paras $
                Box.hsep 1 Box.left
                  (map (paras . map (line . prettyPrintBinderAtom)) (transpose bs))
                  : [line "..." | not b]
            , line "Or alternatively, add a Partial constraint to the type of the enclosing value."
            , line "Non-exhaustive patterns for values without a `Partial` constraint will be disallowed in PureScript 0.9."
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
    renderSimpleErrorMessage (UnusedImport name) =
      line $ "The import of module " ++ runModuleName name ++ " is redundant"

    renderSimpleErrorMessage msg@(UnusedExplicitImport mn names _ _) =
      paras [ line $ "The import of module " ++ runModuleName mn ++ " contains the following unused references:"
            , indent $ paras $ map line names
            , line "It could be replaced with:"
            , indent $ line $ showSuggestion msg ]

    renderSimpleErrorMessage (UnusedDctorImport name) =
      line $ "The import of type " ++ runProperName name ++ " includes data constructors but only the type is used"

    renderSimpleErrorMessage (UnusedDctorExplicitImport name names) =
      paras [ line $ "The import of type " ++ runProperName name ++ " includes the following unused data constructors:"
            , indent $ paras $ map (line .runProperName) names ]

    renderSimpleErrorMessage (DeprecatedOperatorDecl name) =
      paras [ line $ "The operator (" ++ name ++ ") was declared as a value rather than an alias for a named function."
            , line "Operator aliases are declared by using a fixity declaration, for example:"
            , indent $ line $ "infixl 9 someFunction as " ++ name
            , line "Support for value-declared operators will be removed in PureScript 0.9."
            ]

    renderSimpleErrorMessage (DeprecatedOperatorSection op val) =
      paras [ line "An operator section uses legacy syntax. Operator sections are now written using anonymous function syntax:"
            , indent $ foldr1 before $
                case val of
                  Left l ->
                    [ line "("
                    , prettyPrintValue valueDepth l
                    , line " "
                    , renderOperator op
                    , line " _)"
                    ]
                  Right r ->
                    [ line "(_ "
                    , renderOperator op
                    , line " "
                    , prettyPrintValue valueDepth r
                    , line ")"
                    ]
            , line "Support for legacy operator sections will be removed in PureScript 0.9."
            ]
      where
        renderOperator (PositionedValue _ _ ex) = renderOperator ex
        renderOperator (Var (Qualified _ (Op ident))) = line ident
        renderOperator other = Box.hcat Box.top [ line "`", prettyPrintValue valueDepth other, line "`" ]
    renderSimpleErrorMessage (DeprecatedQualifiedSyntax name qualName) =
      paras [ line "Import uses the deprecated 'qualified' syntax:"
            , indent $ line $ "import qualified " ++ runModuleName name ++ " as " ++ runModuleName qualName
            , line "Should instead use the form:"
            , indent $ line $ "import " ++ runModuleName name ++ " as " ++ runModuleName qualName
            , line "The deprecated syntax will be removed in PureScript 0.9."
            ]

    renderSimpleErrorMessage (DeprecatedClassImport mn name) =
      paras [ line $ "Class import from " ++ runModuleName mn ++ " uses deprecated syntax that omits the 'class' keyword:"
            , indent $ line $ runProperName name
            , line "Should instead use the form:"
            , indent $ line $ "class " ++ runProperName name
            , line "The deprecated syntax will be removed in PureScript 0.9."
            ]

    renderSimpleErrorMessage (DeprecatedClassExport name) =
      paras [ line "Class export uses deprecated syntax that omits the 'class' keyword:"
            , indent $ line $ runProperName name
            , line "Should instead use the form:"
            , indent $ line $ "class " ++ runProperName name
            , line "The deprecated syntax will be removed in PureScript 0.9."
            ]

    renderSimpleErrorMessage (DuplicateSelectiveImport name) =
      line $ "There is an existing import of " ++ runModuleName name ++ ", consider merging the import lists"

    renderSimpleErrorMessage (DuplicateImport name imp qual) =
      line $ "Duplicate import of " ++ prettyPrintImport name imp qual

    renderSimpleErrorMessage (DuplicateImportRef ref) =
      line $ "Import list contains multiple references to " ++ ref

    renderSimpleErrorMessage (DuplicateExportRef ref) =
      line $ "Export list contains multiple references to " ++ ref

    renderSimpleErrorMessage (IntOutOfRange value backend lo hi) =
      paras [ line $ "Integer value " ++ show value ++ " is out of range for the " ++ backend ++ " backend."
            , line $ "Acceptable values fall within the range " ++ show lo ++ " to " ++ show hi ++ " (inclusive)." ]

    renderSimpleErrorMessage (RedundantEmptyHidingImport mn) =
      line $ "The import for module " ++ runModuleName mn ++ " is redundant as all members have been explicitly hidden."

    renderSimpleErrorMessage msg@(ImplicitQualifiedImport importedModule asModule _) =
      paras [ line $ "Module " ++ runModuleName importedModule ++ " was imported as " ++ runModuleName asModule ++ " with unspecified imports."
            , line $ "As there are multiple modules being imported as " ++ runModuleName asModule ++ ", consider using the explicit form:"
            , indent $ line $ showSuggestion msg
            ]

    renderSimpleErrorMessage msg@(ImplicitImport mn _) =
      paras [ line $ "Module " ++ runModuleName mn ++ " has unspecified imports, consider using the explicit form: "
            , indent $ line $ showSuggestion msg
            ]

    renderSimpleErrorMessage msg@(HidingImport mn _) =
      paras [ line $ "Module " ++ runModuleName mn ++ " has unspecified imports, consider using the inclusive form: "
            , indent $ line $ showSuggestion msg
            ]

    renderSimpleErrorMessage (CaseBinderLengthDiffers l bs) =
      paras [ line "Binder list length differs in case alternative:"
            , indent $ line $ intercalate ", " $ fmap prettyPrintBinderAtom bs
            , line $ "Expecting " ++ show l ++ " binder" ++ (if l == 1 then "" else "s") ++ "."
            ]

    renderSimpleErrorMessage IncorrectAnonymousArgument =
      line "An anonymous function argument appears in an invalid context."

    renderSimpleErrorMessage (InvalidOperatorInBinder op fn) =
      paras [ line $ "Operator " ++ showIdent op ++ " cannot be used in a pattern as it is an alias for function " ++ showIdent fn ++ "."
            , line "Only aliases for data constructors may be used in patterns."
            ]

    renderSimpleErrorMessage DeprecatedRequirePath =
      line "The require-path option is deprecated and will be removed in PureScript 0.9."

    renderSimpleErrorMessage (CannotGeneralizeRecursiveFunction ident ty) =
      paras [ line $ "Unable to generalize the type of the recursive function " ++ showIdent ident ++ "."
            , line $ "The inferred type of " ++ showIdent ident ++ " was:"
            , indent $ typeAsBox ty
            , line "Try adding a type signature."
            ]

    renderHint :: ErrorMessageHint -> Box.Box -> Box.Box
    renderHint (ErrorUnifyingTypes t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while trying to match type"
                                 , typeAsBox t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "with type"
                                                   , typeAsBox t2
                                                   ]
            ]
    renderHint (ErrorInExpression expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ Box.text "in the expression"
                                 , prettyPrintValue valueDepth expr
                                 ]
            ]
    renderHint (ErrorInModule mn) detail =
      paras [ line $ "in module " ++ runModuleName mn
            , detail
            ]
    renderHint (ErrorInSubsumption t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that type"
                                 , typeAsBox t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "is at least as general as type"
                                                   , typeAsBox t2
                                                   ]
            ]
    renderHint (ErrorInInstance nm ts) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "in type class instance"
                                 , line (showQualified runProperName nm)
                                 , Box.vcat Box.left (map typeAtomAsBox ts)
                                 ]
            ]
    renderHint (ErrorCheckingKind ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking the kind of"
                                 , typeAsBox ty
                                 ]
            ]
    renderHint ErrorCheckingGuard detail =
      paras [ detail
            , line "while checking the type of a guard clause"
            ]
    renderHint (ErrorInferringType expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the type of"
                                 , prettyPrintValue valueDepth expr
                                 ]
            ]
    renderHint (ErrorCheckingType expr ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that expression"
                                 , prettyPrintValue valueDepth expr
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has type"
                                                   , typeAsBox ty
                                                   ]
            ]
    renderHint (ErrorCheckingAccessor expr prop) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking type of property accessor"
                                 , prettyPrintValue valueDepth (Accessor prop expr)
                                 ]
            ]
    renderHint (ErrorInApplication f t a) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while applying a function"
                                 , prettyPrintValue valueDepth f
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "of type"
                                                   , typeAsBox t
                                                   ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "to argument"
                                                   , prettyPrintValue valueDepth a
                                                   ]
            ]
    renderHint (ErrorInDataConstructor nm) detail =
      paras [ detail
            , line $ "in data constructor " ++ runProperName nm
            ]
    renderHint (ErrorInTypeConstructor nm) detail =
      paras [ detail
            , line $ "in type constructor " ++ runProperName nm
            ]
    renderHint (ErrorInBindingGroup nms) detail =
      paras [ detail
            , line $ "in binding group " ++ intercalate ", " (map showIdent nms)
            ]
    renderHint ErrorInDataBindingGroup detail =
      paras [ detail
            , line "in data binding group"
            ]
    renderHint (ErrorInTypeSynonym name) detail =
      paras [ detail
            , line $ "in type synonym " ++ runProperName name
            ]
    renderHint (ErrorInValueDeclaration n) detail =
      paras [ detail
            , line $ "in value declaration " ++ showIdent n
            ]
    renderHint (ErrorInTypeDeclaration n) detail =
      paras [ detail
            , line $ "in type declaration for " ++ showIdent n
            ]
    renderHint (ErrorInForeignImport nm) detail =
      paras [ detail
            , line $ "in foreign import " ++ showIdent nm
            ]
    renderHint (PositionedError srcSpan) detail =
      paras [ line $ "at " ++ displaySourceSpan srcSpan
            , detail
            ]

  valueDepth :: Int
  valueDepth | full = 1000
             | otherwise = 3

  levelText :: String
  levelText = case level of
    Error -> "error"
    Warning -> "warning"

  paras :: [Box.Box] -> Box.Box
  paras = Box.vcat Box.left

  -- | Simplify an error message
  simplifyErrorMessage :: ErrorMessage -> ErrorMessage
  simplifyErrorMessage (ErrorMessage hints simple) = ErrorMessage (simplifyHints hints) simple
    where
    -- Take the last instance of each "hint category"
    simplifyHints :: [ErrorMessageHint] -> [ErrorMessageHint]
    simplifyHints = reverse . nubBy categoriesEqual . stripRedudantHints simple . reverse

    -- Don't remove hints in the "other" category
    categoriesEqual :: ErrorMessageHint -> ErrorMessageHint -> Bool
    categoriesEqual x y =
      case (hintCategory x, hintCategory y) of
        (OtherHint, _) -> False
        (_, OtherHint) -> False
        (c1, c2) -> c1 == c2

    -- | See https://github.com/purescript/purescript/issues/1802
    stripRedudantHints :: SimpleErrorMessage -> [ErrorMessageHint] -> [ErrorMessageHint]
    stripRedudantHints CannotApplyFunction{} = stripFirst isApplicationHint
      where
      isApplicationHint ErrorInApplication{} = True
      isApplicationHint _ = False
    stripRedudantHints ExprDoesNotHaveType{} = stripFirst isCheckHint
      where
      isCheckHint ErrorCheckingType{} = True
      isCheckHint _ = False
    stripRedudantHints TypesDoNotUnify{} = stripFirst isUnifyHint
      where
      isUnifyHint ErrorUnifyingTypes{} = True
      isUnifyHint _ = False
    stripRedudantHints _ = id

    stripFirst :: (ErrorMessageHint -> Bool) -> [ErrorMessageHint] -> [ErrorMessageHint]
    stripFirst p (PositionedError pos : hs) = PositionedError pos : stripFirst p hs
    stripFirst p (ErrorInModule mn    : hs) = ErrorInModule mn    : stripFirst p hs
    stripFirst p (hint                : hs)
      | p hint = hs
      | otherwise = hint : hs
    stripFirst _ [] = []

  hintCategory :: ErrorMessageHint -> HintCategory
  hintCategory ErrorCheckingType{}  = ExprHint
  hintCategory ErrorInferringType{} = ExprHint
  hintCategory ErrorInExpression{}  = ExprHint
  hintCategory ErrorUnifyingTypes{} = CheckHint
  hintCategory ErrorInSubsumption{} = CheckHint
  hintCategory ErrorInApplication{} = CheckHint
  hintCategory ErrorCheckingKind{}  = CheckHint
  hintCategory PositionedError{}    = PositionHint
  hintCategory _                    = OtherHint

-- Pretty print and export declaration
prettyPrintExport :: DeclarationRef -> String
prettyPrintExport (TypeRef pn _) = runProperName pn
prettyPrintExport ref = prettyPrintRef ref

prettyPrintImport :: ModuleName -> ImportDeclarationType -> Maybe ModuleName -> String
prettyPrintImport mn idt qual =
  let i = case idt of
            Implicit -> runModuleName mn
            Explicit refs -> runModuleName mn ++ " (" ++ intercalate ", " (map prettyPrintRef refs) ++ ")"
            Hiding refs -> runModuleName mn ++ " hiding (" ++ intercalate "," (map prettyPrintRef refs) ++ ")"
  in i ++ maybe "" (\q -> " as " ++ runModuleName q) qual

prettyPrintRef :: DeclarationRef -> String
prettyPrintRef (TypeRef pn Nothing) = runProperName pn ++ "(..)"
prettyPrintRef (TypeRef pn (Just [])) = runProperName pn
prettyPrintRef (TypeRef pn (Just dctors)) = runProperName pn ++ "(" ++ intercalate ", " (map runProperName dctors) ++ ")"
prettyPrintRef (TypeOpRef ident) = "type " ++ showIdent ident
prettyPrintRef (ValueRef ident) = showIdent ident
prettyPrintRef (TypeClassRef pn) = "class " ++ runProperName pn
prettyPrintRef (ProperRef name) = name
prettyPrintRef (TypeInstanceRef ident) = showIdent ident
prettyPrintRef (ModuleRef name) = "module " ++ runModuleName name
prettyPrintRef (PositionedDeclarationRef _ _ ref) = prettyPrintExport ref

-- |
-- Pretty print multiple errors
--
prettyPrintMultipleErrors :: Bool -> MultipleErrors -> String
prettyPrintMultipleErrors full = unlines . map renderBox . prettyPrintMultipleErrorsBox full

-- |
-- Pretty print multiple warnings
--
prettyPrintMultipleWarnings :: Bool -> MultipleErrors -> String
prettyPrintMultipleWarnings full = unlines . map renderBox . prettyPrintMultipleWarningsBox full

-- | Pretty print warnings as a Box
prettyPrintMultipleWarningsBox :: Bool -> MultipleErrors -> [Box.Box]
prettyPrintMultipleWarningsBox = prettyPrintMultipleErrorsWith Warning "Warning found:" "Warning"

-- | Pretty print errors as a Box
prettyPrintMultipleErrorsBox :: Bool -> MultipleErrors -> [Box.Box]
prettyPrintMultipleErrorsBox = prettyPrintMultipleErrorsWith Error "Error found:" "Error"

prettyPrintMultipleErrorsWith :: Level -> String -> String -> Bool -> MultipleErrors -> [Box.Box]
prettyPrintMultipleErrorsWith level intro _ full (MultipleErrors [e]) =
  let result = prettyPrintSingleError full level True e
  in [ Box.vcat Box.left [ Box.text intro
                         , result
                         ]
     ]
prettyPrintMultipleErrorsWith level _ intro full (MultipleErrors es) =
  let result = map (prettyPrintSingleError full level True) es
  in concat $ zipWith withIntro [1 :: Int ..] result
  where
  withIntro i err = [ Box.text (intro ++ " " ++ show i ++ " of " ++ show (length es) ++ ":")
                    , Box.moveRight 2 err
                    ]

-- | Pretty print a Parsec ParseError as a Box
prettyPrintParseError :: P.ParseError -> Box.Box
prettyPrintParseError = prettyPrintParseErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . PE.errorMessages

-- |
-- Pretty print ParseError detail messages.
--
-- Adapted from 'Text.Parsec.Error.showErrorMessages', see <https://github.com/aslatter/parsec/blob/v3.1.9/Text/Parsec/Error.hs#L173>.
--
prettyPrintParseErrorMessages :: String -> String -> String -> String -> String -> [Message] -> Box.Box
prettyPrintParseErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
  | null msgs = Box.text msgUnknown
  | otherwise = Box.vcat Box.left $ map Box.text $ clean [showSysUnExpect,showUnExpect,showExpect,showMessages]

  where
  (sysUnExpect,msgs1) = span (SysUnExpect "" ==) msgs
  (unExpect,msgs2)    = span (UnExpect    "" ==) msgs1
  (expect,messages)   = span (Expect      "" ==) msgs2

  showExpect      = showMany msgExpecting expect
  showUnExpect    = showMany msgUnExpected unExpect
  showSysUnExpect | not (null unExpect) ||
                    null sysUnExpect = ""
                  | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                  | otherwise        = msgUnExpected ++ " " ++ firstMsg
    where
    firstMsg  = PE.messageString (head sysUnExpect)

  showMessages      = showMany "" messages

  -- helpers
  showMany pre msgs' = case clean (map PE.messageString msgs') of
                         [] -> ""
                         ms | null pre  -> commasOr ms
                            | otherwise -> pre ++ " " ++ commasOr ms

  commasOr []       = ""
  commasOr [m]      = m
  commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

  commaSep          = separate ", " . clean

  separate   _ []     = ""
  separate   _ [m]    = m
  separate sep (m:ms) = m ++ sep ++ separate sep ms

  clean             = nub . filter (not . null)

-- | Indent to the right, and pad on top and bottom.
indent :: Box.Box -> Box.Box
indent = Box.moveUp 1 . Box.moveDown 1 . Box.moveRight 2

line :: String -> Box.Box
line = Box.text

renderBox :: Box.Box -> String
renderBox = unlines
            . map (dropWhileEnd isSpace)
            . dropWhile whiteSpace
            . dropWhileEnd whiteSpace
            . lines
            . Box.render
  where
  dropWhileEnd p = reverse . dropWhile p . reverse
  whiteSpace = all isSpace

-- |
-- Rethrow an error with a more detailed error message in the case of failure
--
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

reifyErrors :: (MonadError e m) => m a -> m (Either e a)
reifyErrors ma = catchError (fmap Right ma) (return . Left)

reflectErrors :: (MonadError e m) => m (Either e a) -> m a
reflectErrors ma = ma >>= either throwError return

warnAndRethrow :: (MonadError e m, MonadWriter e m) => (e -> e) -> m a -> m a
warnAndRethrow f = rethrow f . censor f

-- |
-- Rethrow an error with source position information
--
rethrowWithPosition :: (MonadError MultipleErrors m) => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (onErrorMessages (withPosition pos))

warnWithPosition :: (MonadWriter MultipleErrors m) => SourceSpan -> m a -> m a
warnWithPosition pos = censor (onErrorMessages (withPosition pos))

warnAndRethrowWithPosition :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => SourceSpan -> m a -> m a
warnAndRethrowWithPosition pos = rethrowWithPosition pos . warnWithPosition pos

withPosition :: SourceSpan -> ErrorMessage -> ErrorMessage
withPosition pos (ErrorMessage hints se) = ErrorMessage (PositionedError pos : hints) se

-- |
-- Runs a computation listening for warnings and then escalating any warnings
-- that match the predicate to error status.
--
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

-- |
-- Collect errors in in parallel
--
parU :: (MonadError MultipleErrors m) => [a] -> (a -> m b) -> m [b]
parU xs f = forM xs (withError . f) >>= collectErrors
  where
  withError :: (MonadError MultipleErrors m) => m a -> m (Either MultipleErrors a)
  withError u = catchError (Right <$> u) (return . Left)

  collectErrors :: (MonadError MultipleErrors m) => [Either MultipleErrors a] -> m [a]
  collectErrors es = case lefts es of
    [] -> return $ rights es
    errs -> throwError $ fold errs
