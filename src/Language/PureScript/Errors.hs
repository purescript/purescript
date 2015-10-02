-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Error
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Errors where

import Data.Either (lefts, rights)
import Data.List (intercalate, transpose)
import Data.Function (on)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (fold, foldMap)
#else
import Data.Foldable (fold)
#endif

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Unify
import Control.Monad.Writer
import Control.Monad.Error.Class (MonadError(..))
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>), Applicative, pure)
#endif
import Control.Monad.Trans.State.Lazy
import Control.Arrow(first)

import Language.PureScript.AST
import Language.PureScript.Environment (isObject, isFunction)
import Language.PureScript.Pretty
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds

import qualified Text.PrettyPrint.Boxes as Box

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE
import Text.Parsec.Error (Message(..))
import Data.List (nub)

-- |
-- A type of error messages
--
data SimpleErrorMessage
  = ErrorParsingExterns P.ParseError
  | ErrorParsingFFIModule FilePath
  | ErrorParsingModule P.ParseError
  | MissingFFIModule ModuleName
  | MultipleFFIModules ModuleName [FilePath]
  | UnnecessaryFFIModule ModuleName FilePath
  | InvalidExternsFile FilePath
  | CannotGetFileInfo FilePath
  | CannotReadFile FilePath
  | CannotWriteFile FilePath
  | InfiniteType Type
  | InfiniteKind Kind
  | CannotReorderOperators
  | MultipleFixities Ident
  | OrphanTypeDeclaration Ident
  | OrphanFixityDeclaration String
  | RedefinedModule ModuleName [SourceSpan]
  | RedefinedIdent Ident
  | OverlappingNamesInLet
  | UnknownModule ModuleName
  | UnknownType (Qualified ProperName)
  | UnknownTypeClass (Qualified ProperName)
  | UnknownValue (Qualified Ident)
  | UnknownDataConstructor (Qualified ProperName) (Maybe (Qualified ProperName))
  | UnknownTypeConstructor (Qualified ProperName)
  | UnknownImportType ModuleName ProperName
  | UnknownExportType ProperName
  | UnknownImportTypeClass ModuleName ProperName
  | UnknownExportTypeClass ProperName
  | UnknownImportValue ModuleName Ident
  | UnknownExportValue Ident
  | UnknownExportModule ModuleName
  | UnknownImportDataConstructor ModuleName ProperName ProperName
  | UnknownExportDataConstructor ProperName ProperName
  | ConflictingImport String ModuleName
  | ConflictingImports String ModuleName ModuleName
  | ConflictingTypeDecls ProperName
  | ConflictingCtorDecls ProperName
  | TypeConflictsWithClass ProperName
  | CtorConflictsWithClass ProperName
  | ClassConflictsWithType ProperName
  | ClassConflictsWithCtor ProperName
  | DuplicateClassExport ProperName
  | DuplicateValueExport Ident
  | DuplicateTypeArgument String
  | InvalidDoBind
  | InvalidDoLet
  | CycleInDeclaration Ident
  | CycleInTypeSynonym (Maybe ProperName)
  | CycleInModules [ModuleName]
  | NameIsUndefined Ident
  | NameNotInScope Ident
  | UndefinedTypeVariable ProperName
  | PartiallyAppliedSynonym (Qualified ProperName)
  | EscapedSkolem (Maybe Expr)
  | UnspecifiedSkolemScope
  | TypesDoNotUnify Type Type
  | KindsDoNotUnify Kind Kind
  | ConstrainedTypeUnified Type Type
  | OverlappingInstances (Qualified ProperName) [Type] [Qualified Ident]
  | NoInstanceFound (Qualified ProperName) [Type]
  | PossiblyInfiniteInstance (Qualified ProperName) [Type]
  | CannotDerive (Qualified ProperName) [Type]
  | CannotFindDerivingType ProperName
  | DuplicateLabel String (Maybe Expr)
  | DuplicateValueDeclaration Ident
  | ArgListLengthsDiffer Ident
  | OverlappingArgNames (Maybe Ident)
  | MissingClassMember Ident
  | ExtraneousClassMember Ident
  | ExpectedType Type Kind
  | IncorrectConstructorArity (Qualified ProperName)
  | SubsumptionCheckFailed
  | ExprDoesNotHaveType Expr Type
  | PropertyIsMissing String Type
  | CannotApplyFunction Type Expr
  | TypeSynonymInstance
  | OrphanInstance Ident (Qualified ProperName) [Type]
  | InvalidNewtype
  | InvalidInstanceHead Type
  | TransitiveExportError DeclarationRef [DeclarationRef]
  | ShadowedName Ident
  | ShadowedTypeVar String
  | UnusedTypeVar String
  | WildcardInferredType Type
  | NotExhaustivePattern [[Binder]] Bool
  | OverlappingPattern [[Binder]] Bool
  | IncompleteExhaustivityCheck
  | ClassOperator ProperName Ident
  | MisleadingEmptyTypeImport ModuleName ProperName
  | ImportHidingModule ModuleName
  deriving (Show)

-- |
-- Wrapper of simpler errors
--
data ErrorMessage
  = NotYetDefined [Ident] ErrorMessage
  | ErrorUnifyingTypes Type Type ErrorMessage
  | ErrorInExpression Expr ErrorMessage
  | ErrorInModule ModuleName ErrorMessage
  | ErrorInInstance (Qualified ProperName) [Type] ErrorMessage
  | ErrorInSubsumption Type Type ErrorMessage
  | ErrorCheckingType Expr Type ErrorMessage
  | ErrorCheckingKind Type ErrorMessage
  | ErrorInferringType Expr ErrorMessage
  | ErrorInApplication Expr Type Expr ErrorMessage
  | ErrorInDataConstructor ProperName ErrorMessage
  | ErrorInTypeConstructor ProperName ErrorMessage
  | ErrorInBindingGroup [Ident] ErrorMessage
  | ErrorInDataBindingGroup ErrorMessage
  | ErrorInTypeSynonym ProperName ErrorMessage
  | ErrorInValueDeclaration Ident ErrorMessage
  | ErrorInTypeDeclaration Ident ErrorMessage
  | ErrorInForeignImport Ident ErrorMessage
  | PositionedError SourceSpan ErrorMessage
  | SimpleErrorWrapper SimpleErrorMessage
  deriving (Show)

instance UnificationError Type ErrorMessage where
  occursCheckFailed t = SimpleErrorWrapper $ InfiniteType t

instance UnificationError Kind ErrorMessage where
  occursCheckFailed k = SimpleErrorWrapper $ InfiniteKind k

-- |
-- Get the error code for a particular error type
--
errorCode :: ErrorMessage -> String
errorCode em = case unwrapErrorMessage em of
  ErrorParsingExterns{} -> "ErrorParsingExterns"
  ErrorParsingFFIModule{} -> "ErrorParsingFFIModule"
  ErrorParsingModule{} -> "ErrorParsingModule"
  MissingFFIModule{} -> "MissingFFIModule"
  MultipleFFIModules{} -> "MultipleFFIModules"
  UnnecessaryFFIModule{} -> "UnnecessaryFFIModule"
  InvalidExternsFile{} -> "InvalidExternsFile"
  CannotGetFileInfo{} -> "CannotGetFileInfo"
  CannotReadFile{} -> "CannotReadFile"
  CannotWriteFile{} -> "CannotWriteFile"
  InfiniteType{} -> "InfiniteType"
  InfiniteKind{} -> "InfiniteKind"
  CannotReorderOperators -> "CannotReorderOperators"
  MultipleFixities{} -> "MultipleFixities"
  OrphanTypeDeclaration{} -> "OrphanTypeDeclaration"
  OrphanFixityDeclaration{} -> "OrphanFixityDeclaration"
  RedefinedModule{} -> "RedefinedModule"
  RedefinedIdent{} -> "RedefinedIdent"
  OverlappingNamesInLet -> "OverlappingNamesInLet"
  UnknownModule{} -> "UnknownModule"
  UnknownType{} -> "UnknownType"
  UnknownTypeClass{} -> "UnknownTypeClass"
  UnknownValue{} -> "UnknownValue"
  UnknownDataConstructor{} -> "UnknownDataConstructor"
  UnknownTypeConstructor{} -> "UnknownTypeConstructor"
  UnknownImportType{} -> "UnknownImportType"
  UnknownExportType{} -> "UnknownExportType"
  UnknownImportTypeClass{} -> "UnknownImportTypeClass"
  UnknownExportTypeClass{} -> "UnknownExportTypeClass"
  UnknownImportValue{} -> "UnknownImportValue"
  UnknownExportValue{} -> "UnknownExportValue"
  UnknownExportModule{} -> "UnknownExportModule"
  UnknownImportDataConstructor{} -> "UnknownImportDataConstructor"
  UnknownExportDataConstructor{} -> "UnknownExportDataConstructor"
  ConflictingImport{} -> "ConflictingImport"
  ConflictingImports{} -> "ConflictingImports"
  ConflictingTypeDecls{} -> "ConflictingTypeDecls"
  ConflictingCtorDecls{} -> "ConflictingCtorDecls"
  TypeConflictsWithClass{} -> "TypeConflictsWithClass"
  CtorConflictsWithClass{} -> "CtorConflictsWithClass"
  ClassConflictsWithType{} -> "ClassConflictsWithType"
  ClassConflictsWithCtor{} -> "ClassConflictsWithCtor"
  DuplicateClassExport{} -> "DuplicateClassExport"
  DuplicateValueExport{} -> "DuplicateValueExport"
  DuplicateTypeArgument{} -> "DuplicateTypeArgument"
  InvalidDoBind -> "InvalidDoBind"
  InvalidDoLet -> "InvalidDoLet"
  CycleInDeclaration{} -> "CycleInDeclaration"
  CycleInTypeSynonym{} -> "CycleInTypeSynonym"
  CycleInModules{} -> "CycleInModules"
  NameIsUndefined{} -> "NameIsUndefined"
  NameNotInScope{} -> "NameNotInScope"
  UndefinedTypeVariable{} -> "UndefinedTypeVariable"
  PartiallyAppliedSynonym{} -> "PartiallyAppliedSynonym"
  EscapedSkolem{} -> "EscapedSkolem"
  UnspecifiedSkolemScope -> "UnspecifiedSkolemScope"
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
  SubsumptionCheckFailed -> "SubsumptionCheckFailed"
  ExprDoesNotHaveType{} -> "ExprDoesNotHaveType"
  PropertyIsMissing{} -> "PropertyIsMissing"
  CannotApplyFunction{} -> "CannotApplyFunction"
  TypeSynonymInstance -> "TypeSynonymInstance"
  OrphanInstance{} -> "OrphanInstance"
  InvalidNewtype -> "InvalidNewtype"
  InvalidInstanceHead{} -> "InvalidInstanceHead"
  TransitiveExportError{} -> "TransitiveExportError"
  ShadowedName{} -> "ShadowedName"
  ShadowedTypeVar{} -> "ShadowedTypeVar"
  UnusedTypeVar{} -> "UnusedTypeVar"
  WildcardInferredType{} -> "WildcardInferredType"
  NotExhaustivePattern{} -> "NotExhaustivePattern"
  OverlappingPattern{} -> "OverlappingPattern"
  IncompleteExhaustivityCheck{} -> "IncompleteExhaustivityCheck"
  ClassOperator{} -> "ClassOperator"
  MisleadingEmptyTypeImport{} -> "MisleadingEmptyTypeImport"
  ImportHidingModule{} -> "ImportHidingModule"

-- |
-- A stack trace for an error
--
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage] } deriving (Show, Monoid)

instance UnificationError Type MultipleErrors where
  occursCheckFailed t = MultipleErrors [occursCheckFailed t]

instance UnificationError Kind MultipleErrors where
  occursCheckFailed k = MultipleErrors [occursCheckFailed k]

-- | Check whether a collection of errors is empty or not.
nonEmpty :: MultipleErrors -> Bool
nonEmpty = not . null . runMultipleErrors

-- |
-- Create an error set from a single simple error message
--
errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [SimpleErrorWrapper err]


-- |
-- Create an error set from a single error message
--
singleError :: ErrorMessage -> MultipleErrors
singleError = MultipleErrors . pure

-- |
-- Lift a function on ErrorMessage to a function on MultipleErrors
--
onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f = MultipleErrors . map f . runMultipleErrors

-- | The various types of things which might need to be relabelled in errors messages.
data LabelType = TypeLabel | SkolemLabel String deriving (Show, Read, Eq, Ord)

-- | A map from rigid type variable name/unknown variable pairs to new variables.
type UnknownMap = M.Map (LabelType, Unknown) Unknown

-- | How critical the issue is
data Level = Error | Warning deriving Show

-- |
-- Extract nested error messages from wrapper errors
--
unwrapErrorMessage :: ErrorMessage -> SimpleErrorMessage
unwrapErrorMessage em = case em of
  (ErrorCheckingKind _ err)       -> unwrapErrorMessage err
  (ErrorCheckingType _ _ err)     -> unwrapErrorMessage err
  (ErrorInApplication _ _ _ err)  -> unwrapErrorMessage err
  (ErrorInBindingGroup _ err)     -> unwrapErrorMessage err
  (ErrorInDataBindingGroup err)   -> unwrapErrorMessage err
  (ErrorInDataConstructor _ err)  -> unwrapErrorMessage err
  (ErrorInExpression _ err)       -> unwrapErrorMessage err
  (ErrorInForeignImport _ err)    -> unwrapErrorMessage err
  (ErrorInInstance _ _ err)       -> unwrapErrorMessage err
  (ErrorInModule _ err)           -> unwrapErrorMessage err
  (ErrorInSubsumption _ _ err)    -> unwrapErrorMessage err
  (ErrorInTypeConstructor _ err)  -> unwrapErrorMessage err
  (ErrorInTypeSynonym _ err)      -> unwrapErrorMessage err
  (ErrorInValueDeclaration _ err) -> unwrapErrorMessage err
  (ErrorInTypeDeclaration _ err)  -> unwrapErrorMessage err
  (ErrorInferringType _ err)      -> unwrapErrorMessage err
  (ErrorUnifyingTypes _ _ err)    -> unwrapErrorMessage err
  (NotYetDefined _ err)           -> unwrapErrorMessage err
  (PositionedError _ err)         -> unwrapErrorMessage err
  (SimpleErrorWrapper sem)        -> sem

replaceUnknowns :: Type -> State UnknownMap Type
replaceUnknowns = everywhereOnTypesM replaceTypes
  where
  lookupTable :: (LabelType, Unknown) -> UnknownMap -> (Unknown, UnknownMap)
  lookupTable x m = case M.lookup x m of
                      Nothing -> let i = length (filter (on (==) fst x) (M.keys m)) in (i, M.insert x i m)
                      Just i  -> (i, m)

  replaceTypes :: Type -> State UnknownMap Type
  replaceTypes (TUnknown u) = state $ first TUnknown . lookupTable (TypeLabel, u)
  replaceTypes (Skolem name s sko) = state $ first (flip (Skolem name) sko) . lookupTable (SkolemLabel name, s)
  replaceTypes other = return other

onTypesInErrorMessageM :: (Applicative m) => (Type -> m Type) -> ErrorMessage -> m ErrorMessage
onTypesInErrorMessageM f = g
  where
    gSimple (InfiniteType t) = InfiniteType <$> (f t)
    gSimple (TypesDoNotUnify t1 t2) = TypesDoNotUnify <$> (f t1) <*> (f t2)
    gSimple (ConstrainedTypeUnified t1 t2) = ConstrainedTypeUnified <$> (f t1) <*> (f t2)
    gSimple (ExprDoesNotHaveType e t) = ExprDoesNotHaveType e <$> (f t)
    gSimple (PropertyIsMissing s t) = PropertyIsMissing s <$> (f t)
    gSimple (CannotApplyFunction t e) = CannotApplyFunction <$> f t <*> (pure e)
    gSimple (InvalidInstanceHead t) = InvalidInstanceHead <$> f t
    gSimple other = pure other
    g (ErrorInSubsumption t1 t2 em) = ErrorInSubsumption <$> (f t1) <*> (f t2) <*> (g em)
    g (ErrorUnifyingTypes t1 t2 e) = ErrorUnifyingTypes <$> (f t1) <*> (f t2) <*> (g e)
    g (ErrorCheckingType e t em) = ErrorCheckingType e <$> (f t) <*> (g em)
    g (ErrorCheckingKind t em) = ErrorCheckingKind <$> (f t) <*> g em
    g (ErrorInApplication e1 t1 e2 em) = ErrorInApplication e1 <$> (f t1) <*> (pure e2) <*> (g em)
    g (NotYetDefined x e) = NotYetDefined x <$> (g e)
    g (ErrorInExpression x e) = ErrorInExpression x <$> (g e)
    g (ErrorInModule x e) = ErrorInModule x <$> (g e)
    g (ErrorInInstance x y e) = ErrorInInstance x y <$> (g e)
    g (ErrorInferringType x e) = ErrorInferringType x <$> (g e)
    g (ErrorInDataConstructor x e) = ErrorInDataConstructor x <$> (g e)
    g (ErrorInTypeConstructor x e) = ErrorInTypeConstructor x <$> (g e)
    g (ErrorInBindingGroup x e) = ErrorInBindingGroup x <$> (g e)
    g (ErrorInDataBindingGroup e) = ErrorInDataBindingGroup <$> (g e)
    g (ErrorInTypeSynonym x e) = ErrorInTypeSynonym x <$> (g e)
    g (ErrorInValueDeclaration x e) = ErrorInValueDeclaration x <$> (g e)
    g (ErrorInTypeDeclaration x e) = ErrorInTypeDeclaration x <$> (g e)
    g (ErrorInForeignImport x e) = ErrorInForeignImport x <$> (g e)
    g (PositionedError x e) = PositionedError x <$> (g e)
    g (SimpleErrorWrapper sem) = SimpleErrorWrapper <$> gSimple sem

-- |
-- Pretty print a single error, simplifying if necessary
--
prettyPrintSingleError :: Bool -> Level -> ErrorMessage -> State UnknownMap Box.Box
prettyPrintSingleError full level e = prettyPrintErrorMessage <$> onTypesInErrorMessageM replaceUnknowns (if full then e else simplifyErrorMessage e)
 where
  -- |
  -- Pretty print an ErrorMessage
  --
  prettyPrintErrorMessage :: ErrorMessage -> Box.Box
  prettyPrintErrorMessage em =
    paras $
      go em:suggestions em ++
      [line $ "See " ++ wikiUri ++ " for more information, or to contribute content related to this " ++ levelText ++ "."]
    where
    wikiUri :: String
    wikiUri = "https://github.com/purescript/purescript/wiki/Error-Code-" ++ errorCode e

    go :: ErrorMessage -> Box.Box
    goSimple (CannotGetFileInfo path) =
      paras [ line "Unable to read file info: "
            , indent . line $ path
            ]
    goSimple (CannotReadFile path) =
      paras [ line "Unable to read file: "
            , indent . line $ path
            ]
    goSimple (CannotWriteFile path) =
      paras [ line "Unable to write file: "
            , indent . line $ path
            ]
    goSimple (ErrorParsingExterns err) =
      paras [ lineWithLevel "parsing externs files: "
            , indent . prettyPrintParseError $ err
            ]
    goSimple (ErrorParsingFFIModule path) =
      paras [ line "Unable to parse module from FFI file: "
            , indent . line $ path
            ]
    goSimple (ErrorParsingModule err) =
      paras [ line "Unable to parse module: "
            , indent . prettyPrintParseError $ err
            ]
    goSimple (MissingFFIModule mn) =
      line $ "Missing FFI implementations for module " ++ runModuleName mn
    goSimple (UnnecessaryFFIModule mn path) =
      paras [ line $ "Unnecessary FFI implementations have been provided for module " ++ runModuleName mn ++ ": "
            , indent . line $ path
            ]
    goSimple (MultipleFFIModules mn paths) =
      paras $ [ line $ "Multiple FFI implementations have been provided for module " ++ runModuleName mn ++ ": " ]
            ++ map (indent . line) paths
    goSimple (InvalidExternsFile path) =
      paras [ line "Externs file is invalid: "
            , indent . line $ path
            ]
    goSimple InvalidDoBind =
      line "Bind statement cannot be the last statement in a do block. The last statement must be an expression."
    goSimple InvalidDoLet =
      line "Let statement cannot be the last statement in a do block. The last statement must be an expression."
    goSimple CannotReorderOperators =
      line "Unable to reorder operators"
    goSimple UnspecifiedSkolemScope =
      line "Skolem variable scope is unspecified"
    goSimple OverlappingNamesInLet =
      line "Overlapping names in let binding."
    goSimple (InfiniteType ty) =
      paras [ line "An infinite type was inferred for an expression: "
            , indent $ line $ prettyPrintType ty
            ]
    goSimple (InfiniteKind ki) =
      paras [ line "An infinite kind was inferred for a type: "
            , indent $ line $ prettyPrintKind ki
            ]
    goSimple (MultipleFixities name) =
      line $ "Multiple fixity declarations for " ++ showIdent name
    goSimple (OrphanTypeDeclaration nm) =
      line $ "Orphan type declaration for " ++ showIdent nm
    goSimple (OrphanFixityDeclaration op) =
      line $ "Orphan fixity declaration for " ++ show op
    goSimple (RedefinedModule name filenames) =
      paras $ line ("Module " ++ runModuleName name ++ " has been defined multiple times:")
              : map (indent . line . displaySourceSpan) filenames
    goSimple (RedefinedIdent name) =
      line $ "Name " ++ showIdent name ++ " has been defined multiple times"
    goSimple (UnknownModule mn) =
      line $ "Unknown module " ++ runModuleName mn
    goSimple (UnknownType name) =
      line $ "Unknown type " ++ showQualified runProperName name
    goSimple (UnknownTypeClass name) =
      line $ "Unknown type class " ++ showQualified runProperName name
    goSimple (UnknownValue name) =
      line $ "Unknown value " ++ showQualified showIdent name
    goSimple (UnknownTypeConstructor name) =
      line $ "Unknown type constructor " ++ showQualified runProperName name
    goSimple (UnknownDataConstructor dc tc) =
      line $ "Unknown data constructor " ++ showQualified runProperName dc ++ foldMap ((" for type constructor " ++) . showQualified runProperName) tc
    goSimple (UnknownImportType mn name) =
      line $ "Module " ++ runModuleName mn ++ " does not export type " ++ runProperName name
    goSimple (UnknownExportType name) =
      line $ "Cannot export unknown type " ++ runProperName name
    goSimple (UnknownImportTypeClass mn name) =
      line $ "Module " ++ runModuleName mn ++ " does not export type class " ++ runProperName name
    goSimple (UnknownExportTypeClass name) =
      line $ "Cannot export unknown type class " ++ runProperName name
    goSimple (UnknownImportValue mn name) =
      line $ "Module " ++ runModuleName mn ++ " does not export value " ++ showIdent name
    goSimple (UnknownExportValue name) =
      line $ "Cannot export unknown value " ++ showIdent name
    goSimple (UnknownExportModule name) =
      line $ "Cannot export unknown module " ++ runModuleName name ++ ", it either does not exist or has not been imported by the current module"
    goSimple (UnknownImportDataConstructor mn tcon dcon) =
      line $ "Module " ++ runModuleName mn ++ " does not export data constructor " ++ runProperName dcon ++ " for type " ++ runProperName tcon
    goSimple (UnknownExportDataConstructor tcon dcon) =
      line $ "Cannot export data constructor " ++ runProperName dcon ++ " for type " ++ runProperName tcon ++ " as it has not been declared"
    goSimple (ConflictingImport nm mn) =
      line $ "Cannot declare " ++ show nm ++ " since another declaration of that name was imported from " ++ runModuleName mn
    goSimple (ConflictingImports nm m1 m2) =
      line $ "Conflicting imports for " ++ nm ++ " from modules " ++ runModuleName m1 ++ " and " ++ runModuleName m2
    goSimple (ConflictingTypeDecls nm) =
      line $ "Conflicting type declarations for " ++ runProperName nm
    goSimple (ConflictingCtorDecls nm) =
      line $ "Conflicting data constructor declarations for " ++ runProperName nm
    goSimple (TypeConflictsWithClass nm) =
      line $ "Type " ++ runProperName nm ++ " conflicts with type class declaration of the same name"
    goSimple (CtorConflictsWithClass nm) =
      line $ "Data constructor " ++ runProperName nm ++ " conflicts with type class declaration of the same name"
    goSimple (ClassConflictsWithType nm) =
      line $ "Type class " ++ runProperName nm ++ " conflicts with type declaration of the same name"
    goSimple (ClassConflictsWithCtor nm) =
      line $ "Type class " ++ runProperName nm ++ " conflicts with data constructor declaration of the same name"
    goSimple (DuplicateClassExport nm) =
      line $ "Duplicate export declaration for type class " ++ runProperName nm
    goSimple (DuplicateValueExport nm) =
      line $ "Duplicate export declaration for value " ++ showIdent nm
    goSimple (CycleInDeclaration nm) =
      line $ "Cycle in declaration of " ++ showIdent nm
    goSimple (CycleInModules mns) =
      line $ "Cycle in module dependencies: " ++ intercalate ", " (map runModuleName mns)
    goSimple (CycleInTypeSynonym pn) =
      line $ "Cycle in type synonym" ++ foldMap ((" " ++) . runProperName) pn
    goSimple (NameIsUndefined ident) =
      line $ showIdent ident ++ " is undefined"
    goSimple (NameNotInScope ident) =
      line $ showIdent ident ++ " may not be defined in the current scope"
    goSimple (UndefinedTypeVariable name) =
      line $ "Type variable " ++ runProperName name ++ " is undefined"
    goSimple (PartiallyAppliedSynonym name) =
      paras [ line $ "Partially applied type synonym " ++ showQualified runProperName name
            , line "Type synonyms must be applied to all of their type arguments."
            ]
    goSimple (EscapedSkolem binding) =
      paras $ [ line "A type variable has escaped its scope." ]
                     <> foldMap (\expr -> [ line "Relevant expression: "
                                          , indent $ line $ prettyPrintValue expr
                                          ]) binding
    goSimple (TypesDoNotUnify t1 t2)
      = paras [ line "Cannot unify type"
              , indent $ line $ prettyPrintType t1
              , line "with type"
              , indent $ line $ prettyPrintType t2
              ]
    goSimple (KindsDoNotUnify k1 k2) =
      paras [ line "Cannot unify kind"
            , indent $ line $ prettyPrintKind k1
            , line "with kind"
            , indent $ line $ prettyPrintKind k2
            ]
    goSimple (ConstrainedTypeUnified t1 t2) =
      paras [ line "Cannot unify constrained type"
            , indent $ line $ prettyPrintType t1
            , line "with type"
            , indent $ line $ prettyPrintType t2
            ]
    goSimple (OverlappingInstances nm ts (d : ds)) =
      paras [ line $ "Overlapping instances found for " ++ showQualified runProperName nm ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":"
            , indent $ paras (line (showQualified showIdent d ++ " (chosen)") : map (line . showQualified showIdent) ds)
            ]
    goSimple OverlappingInstances{} = error "OverlappingInstances: empty instance list"
    goSimple (NoInstanceFound nm ts) =
      line $ "No instance found for " ++ showQualified runProperName nm ++ " " ++ unwords (map prettyPrintTypeAtom ts)
    goSimple (PossiblyInfiniteInstance nm ts) =
      line $ "Instance for " ++ showQualified runProperName nm ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ " is possibly infinite."
    goSimple (CannotDerive nm ts) =
      line $ "Cannot derive " ++ showQualified runProperName nm ++ " instance for " ++ unwords (map prettyPrintTypeAtom ts)
    goSimple (CannotFindDerivingType nm) =
      line $ "Cannot derive instance, because the type declaration for " ++ runProperName nm ++ " could not be found."
    goSimple (DuplicateLabel l expr) =
      paras $ [ line $ "Duplicate label " ++ show l ++ " in row." ]
                       <> foldMap (\expr' -> [ line "Relevant expression: "
                                             , indent $ line $ prettyPrintValue expr'
                                             ]) expr
    goSimple (DuplicateTypeArgument name) =
      line $ "Duplicate type argument " ++ show name
    goSimple (DuplicateValueDeclaration nm) =
      line $ "Duplicate value declaration for " ++ showIdent nm
    goSimple (ArgListLengthsDiffer ident) =
      line $ "Argument list lengths differ in declaration " ++ showIdent ident
    goSimple (OverlappingArgNames ident) =
      line $ "Overlapping names in function/binder" ++ foldMap ((" in declaration" ++) . showIdent) ident
    goSimple (MissingClassMember ident) =
      line $ "Member " ++ showIdent ident ++ " has not been implemented"
    goSimple (ExtraneousClassMember ident) =
      line $ "Member " ++ showIdent ident ++ " is not a member of the class being instantiated"
    goSimple (ExpectedType ty kind) =
      paras [ line "In a type-annotated expression x :: t, the type t must have kind *."
            , line $ "The error arises from the type " ++ prettyPrintType ty ++ " having the kind "  ++ prettyPrintKind kind ++ " instead."
            ]
    goSimple (IncorrectConstructorArity nm) =
      line $ "Wrong number of arguments to constructor " ++ showQualified runProperName nm
    goSimple SubsumptionCheckFailed = line "Unable to check type subsumption"
    goSimple (ExprDoesNotHaveType expr ty) =
      paras [ line "Expression"
            , indent $ line $ prettyPrintValue expr
            , line "does not have type"
            , indent $ line $ prettyPrintType ty
            ]
    goSimple (PropertyIsMissing prop row) =
      line $ "Row " ++ prettyPrintRow row ++ " lacks required property " ++ show prop
    goSimple (CannotApplyFunction fn arg) =
      paras [ line "Cannot apply function of type"
            , indent $ line $ prettyPrintType fn
            , line "to argument"
            , indent $ line $ prettyPrintValue arg
            ]
    goSimple TypeSynonymInstance =
      line "Type synonym instances are disallowed"
    goSimple (OrphanInstance nm cnm ts) =
      paras [ line $ "Instance " ++ showIdent nm ++ " for " ++ showQualified runProperName cnm ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ " is an orphan instance."
            , line "An orphan instance is an instance which is defined in neither the class module nor the data type module."
            , line "Consider moving the instance, if possible, or using a newtype wrapper."
            ]
    goSimple InvalidNewtype =
      line "Newtypes must define a single constructor with a single argument"
    goSimple (InvalidInstanceHead ty) =
      paras [ line "Invalid type in class instance head:"
            , indent $ line $ prettyPrintType ty
            ]
    goSimple (TransitiveExportError x ys) =
      paras $ line ("An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: ")
              : map (line . prettyPrintExport) ys
    goSimple (ShadowedName nm) =
      line $ "Name '" ++ showIdent nm ++ "' was shadowed"
    goSimple (ShadowedTypeVar tv) =
      line $ "Type variable '" ++ tv ++ "' was shadowed"
    goSimple (UnusedTypeVar tv) =
      line $ "Type variable '" ++ tv ++ "' was declared but not used"
    goSimple (ClassOperator className opName) =
      paras [ line $ "Class '" ++ runProperName className ++ "' declares operator " ++ showIdent opName ++ "."
            , indent $ line "This may be disallowed in the future - consider declaring a named member in the class and making the operator an alias:"
            , indent $ line $ showIdent opName ++ " = someMember"
            ]
    goSimple (MisleadingEmptyTypeImport mn name) =
      line $ "Importing type " ++ runProperName name ++ "(..) from " ++ runModuleName mn ++ " is misleading as it has no exported data constructors"
    goSimple (ImportHidingModule name) =
      line $ "Attempted to hide module " ++ runModuleName name ++ " in import expression, this is not permitted"
    goSimple (WildcardInferredType ty) =
      line $ "The wildcard type definition has the inferred type " ++ prettyPrintType ty
    goSimple (NotExhaustivePattern bs b) =
      paras $ [ line "A case expression could not be determined to cover all inputs."
              , line "The following additional cases are required to cover all inputs:\n"
              , Box.hsep 1 Box.left (map (paras . map (line . prettyPrintBinderAtom)) (transpose bs))
              ] ++
              [ line "..." | not b ]
    goSimple (OverlappingPattern bs b) =
      paras $ [ line "A case expression contains unreachable cases:\n"
              , Box.hsep 1 Box.left (map (paras . map (line . prettyPrintBinderAtom)) (transpose bs))
              ] ++
              [ line "..." | not b ]
    goSimple IncompleteExhaustivityCheck =
      paras [ line "An exhaustivity check was abandoned due to too many possible cases."
            , line "You may want to decomposing your data types into smaller types."
            ]
    go (NotYetDefined names err) =
      paras [ line $ "The following are not yet defined here: " ++ intercalate ", " (map showIdent names) ++ ":"
            , indent $ go err
            ]
    go (ErrorUnifyingTypes t1 t2 err) =
      paras [ lineWithLevel "unifying type "
            , indent $ line $ prettyPrintType t1
            , line "with type"
            , indent $ line $ prettyPrintType t2
            , go err
            ]
    go (ErrorInExpression expr err) =
      paras [ lineWithLevel "in expression:"
            , indent $ line $ prettyPrintValue expr
            , go err
            ]
    go (ErrorInModule mn err) =
      paras [ lineWithLevel $ "in module " ++ runModuleName mn ++ ":"
            , go err
            ]
    go (ErrorInSubsumption t1 t2 err) =
      paras [ lineWithLevel "checking that type "
            , indent $ line $ prettyPrintType t1
            , line "subsumes type"
            , indent $ line $ prettyPrintType t2
            , go err
            ]
    go (ErrorInInstance name ts err) =
      paras [ lineWithLevel $ "in type class instance " ++ showQualified runProperName name ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":"
            , go err
            ]
    go (ErrorCheckingKind ty err) =
      paras [ lineWithLevel "checking kind of type "
            , indent $ line $ prettyPrintType ty
            , go err
            ]
    go (ErrorInferringType expr err) =
      paras [ lineWithLevel "inferring type of value "
            , indent $ line $ prettyPrintValue expr
            , go err
            ]
    go (ErrorCheckingType expr ty err) =
      paras [ lineWithLevel "checking that value "
            , indent $ line $ prettyPrintValue expr
            , line "has type"
            , indent $ line $ prettyPrintType ty
            , go err
            ]
    go (ErrorInApplication f t a err) =
      paras [ lineWithLevel "applying function"
            , indent $ line $ prettyPrintValue f
            , line "of type"
            , indent $ line $ prettyPrintType t
            , line "to argument"
            , indent $ line $ prettyPrintValue a
            , go err
            ]
    go (ErrorInDataConstructor nm err) =
      paras [ lineWithLevel $ "in data constructor " ++ runProperName nm ++ ":"
            , go err
            ]
    go (ErrorInTypeConstructor nm err) =
      paras [ lineWithLevel $ "in type constructor " ++ runProperName nm ++ ":"
            , go err
            ]
    go (ErrorInBindingGroup nms err) =
      paras [ lineWithLevel $ "in binding group " ++ intercalate ", " (map showIdent nms) ++ ":"
            , go err
            ]
    go (ErrorInDataBindingGroup err) =
      paras [ lineWithLevel $ "in data binding group:"
            , go err
            ]
    go (ErrorInTypeSynonym name err) =
      paras [ lineWithLevel $ "in type synonym " ++ runProperName name ++ ":"
            , go err
            ]
    go (ErrorInValueDeclaration n err) =
      paras [ lineWithLevel $ "in value declaration " ++ showIdent n ++ ":"
            , go err
            ]
    go (ErrorInTypeDeclaration n err) =
      paras [ lineWithLevel $ "in type declaration for " ++ showIdent n ++ ":"
            , go err
            ]
    go (ErrorInForeignImport nm err) =
      paras [ lineWithLevel $ "in foreign import " ++ showIdent nm ++ ":"
            , go err
            ]
    go (PositionedError srcSpan err) =
      paras [ lineWithLevel $ "at " ++ displaySourceSpan srcSpan ++ ":"
            , indent $ go err
            ]
    go (SimpleErrorWrapper sem) = goSimple sem

  lineWithLevel :: String -> Box.Box
  lineWithLevel text = line $ show level ++ " " ++ text

  levelText :: String
  levelText = case level of
    Error -> "error"
    Warning -> "warning"

  suggestions :: ErrorMessage -> [Box.Box]
  suggestions = suggestions' . unwrapErrorMessage
    where
    suggestions' (ConflictingImport nm im) = [ line $ "Possible fix: hide " ++ show nm ++ " when importing " ++ runModuleName im ++ ":"
                                             , indent . line $ "import " ++ runModuleName im ++ " hiding (" ++ nm ++ ")"
                                             ]
    suggestions' (TypesDoNotUnify t1 t2)
      | isObject t1 && isFunction t2 = [line "Note that function composition in PureScript is defined using (<<<)"]
      | otherwise             = []
    suggestions' _ = []

  paras :: [Box.Box] -> Box.Box
  paras = Box.vcat Box.left

  -- |
  -- Pretty print and export declaration
  --
  prettyPrintExport :: DeclarationRef -> String
  prettyPrintExport (TypeRef pn _) = runProperName pn
  prettyPrintExport (ValueRef ident) = showIdent ident
  prettyPrintExport (TypeClassRef pn) = runProperName pn
  prettyPrintExport (TypeInstanceRef ident) = showIdent ident
  prettyPrintExport (ModuleRef name) = "module " ++ runModuleName name
  prettyPrintExport (PositionedDeclarationRef _ _ ref) = prettyPrintExport ref

  -- |
  -- Simplify an error message
  --
  simplifyErrorMessage :: ErrorMessage -> ErrorMessage
  simplifyErrorMessage = unwrap Nothing
    where
    unwrap :: Maybe SourceSpan -> ErrorMessage -> ErrorMessage
    unwrap pos (ErrorInExpression _ err) = unwrap pos err
    unwrap pos (ErrorInInstance name ts err) = ErrorInInstance name ts (unwrap pos err)
    unwrap pos (ErrorInSubsumption t1 t2 err) = ErrorInSubsumption t1 t2 (unwrap pos err)
    unwrap pos (ErrorUnifyingTypes _ _ err) = unwrap pos err
    unwrap pos (ErrorInferringType _ err) = unwrap pos err
    unwrap pos (ErrorCheckingType _ _ err) = unwrap pos err
    unwrap pos (ErrorCheckingKind ty err) = ErrorCheckingKind ty (unwrap pos err)
    unwrap pos (ErrorInModule mn err) = ErrorInModule mn (unwrap pos err)
    unwrap pos (ErrorInApplication _ _ _ err) = unwrap pos err
    unwrap pos (ErrorInDataConstructor nm err) = ErrorInDataConstructor nm (unwrap pos err)
    unwrap pos (ErrorInTypeConstructor nm err) = ErrorInTypeConstructor nm (unwrap pos err)
    unwrap pos (ErrorInBindingGroup nms err) = ErrorInBindingGroup nms (unwrap pos err)
    unwrap pos (ErrorInDataBindingGroup err) = ErrorInDataBindingGroup (unwrap pos err)
    unwrap pos (ErrorInTypeSynonym nm err) = ErrorInTypeSynonym nm (unwrap pos err)
    unwrap pos (ErrorInValueDeclaration nm err) = ErrorInValueDeclaration nm (unwrap pos err)
    unwrap pos (ErrorInTypeDeclaration nm err) = ErrorInTypeDeclaration nm (unwrap pos err)
    unwrap pos (ErrorInForeignImport nm err) = ErrorInForeignImport nm (unwrap pos err)
    unwrap pos (NotYetDefined ns err) = NotYetDefined ns (unwrap pos err)
    unwrap _   (PositionedError pos err) = unwrap (Just pos) err
    unwrap pos other = wrap pos other

    wrap :: Maybe SourceSpan -> ErrorMessage -> ErrorMessage
    wrap Nothing    = id
    wrap (Just pos) = PositionedError pos


-- |
-- Pretty print multiple errors
--
prettyPrintMultipleErrors :: Bool -> MultipleErrors -> String
prettyPrintMultipleErrors full = renderBox . prettyPrintMultipleErrorsBox full

-- |
-- Pretty print multiple warnings
--
prettyPrintMultipleWarnings :: Bool -> MultipleErrors ->  String
prettyPrintMultipleWarnings full = renderBox . prettyPrintMultipleWarningsBox full

-- | Pretty print warnings as a Box
prettyPrintMultipleWarningsBox :: Bool -> MultipleErrors -> Box.Box
prettyPrintMultipleWarningsBox full = flip evalState M.empty . prettyPrintMultipleErrorsWith Warning "Warning found:" "Multiple warnings found:" full

-- | Pretty print errors as a Box
prettyPrintMultipleErrorsBox :: Bool -> MultipleErrors -> Box.Box
prettyPrintMultipleErrorsBox full = flip evalState M.empty . prettyPrintMultipleErrorsWith Error "Error found:" "Multiple errors found:" full

prettyPrintMultipleErrorsWith :: Level -> String -> String -> Bool -> MultipleErrors -> State UnknownMap Box.Box
prettyPrintMultipleErrorsWith level intro _ full  (MultipleErrors [e]) = do
  result <- prettyPrintSingleError full level e
  return $
    Box.vcat Box.left [ Box.text intro
                      , result
                      ]
prettyPrintMultipleErrorsWith level _ intro full  (MultipleErrors es) = do
  result <- forM es $ (liftM $ Box.moveRight 2) . prettyPrintSingleError full level
  return $
    Box.vcat Box.left [ Box.text intro
                      , Box.vsep 1 Box.left result
                      ]

-- | Pretty print a Parsec ParseError as a Box
prettyPrintParseError :: P.ParseError -> Box.Box
prettyPrintParseError = (prettyPrintParseErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input") . PE.errorMessages

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
  (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
  (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
  (expect,messages)   = span ((Expect      "") ==) msgs2

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

indent :: Box.Box -> Box.Box
indent = Box.moveRight 2

line :: String -> Box.Box
line = Box.text

renderBox :: Box.Box -> String
renderBox = unlines . map trimEnd . lines . Box.render
  where
  trimEnd = reverse . dropWhile (== ' ') . reverse

-- |
-- Interpret multiple errors and warnings in a monad supporting errors and warnings
--
interpretMultipleErrorsAndWarnings :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => (Either MultipleErrors a, MultipleErrors) -> m a
interpretMultipleErrorsAndWarnings (err, ws) = do
  tell ws
  either throwError return $ err

-- |
-- Rethrow an error with a more detailed error message in the case of failure
--
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

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
withPosition _ (PositionedError pos err) = withPosition pos err
withPosition pos err = PositionedError pos err

-- |
-- Collect errors in in parallel
--
parU :: (MonadError MultipleErrors m, Functor m) => [a] -> (a -> m b) -> m [b]
parU xs f = forM xs (withError . f) >>= collectErrors
  where
  withError :: (MonadError MultipleErrors m, Functor m) => m a -> m (Either MultipleErrors a)
  withError u = catchError (Right <$> u) (return . Left)

  collectErrors :: (MonadError MultipleErrors m, Functor m) => [Either MultipleErrors a] -> m [a]
  collectErrors es = case lefts es of
    [] -> return $ rights es
    errs -> throwError $ fold errs
