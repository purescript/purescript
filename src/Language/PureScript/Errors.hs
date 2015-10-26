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
import Data.List (intercalate, transpose, nub, nubBy)
import Data.Function (on)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (fold, foldMap)
import Data.Traversable (traverse)
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

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Pretty
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds

import qualified Text.PrettyPrint.Boxes as Box

import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE
import Text.Parsec.Error (Message(..))

-- | A type of error messages
data SimpleErrorMessage
  = ErrorParsingFFIModule FilePath
  | ErrorParsingModule P.ParseError
  | MissingFFIModule ModuleName
  | MultipleFFIModules ModuleName [FilePath]
  | UnnecessaryFFIModule ModuleName FilePath
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
  | DuplicateModuleName ModuleName
  | DuplicateClassExport ProperName
  | DuplicateValueExport Ident
  | DuplicateTypeArgument String
  | InvalidDoBind
  | InvalidDoLet
  | CycleInDeclaration Ident
  | CycleInTypeSynonym (Maybe ProperName)
  | CycleInModules [ModuleName]
  | NameIsUndefined Ident
  | UndefinedTypeVariable ProperName
  | PartiallyAppliedSynonym (Qualified ProperName)
  | EscapedSkolem (Maybe Expr)
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
  | ExtraneousClassMember Ident (Qualified ProperName)
  | ExpectedType Type Kind
  | IncorrectConstructorArity (Qualified ProperName)
  | ExprDoesNotHaveType Expr Type
  | PropertyIsMissing String
  | AdditionalProperty String
  | CannotApplyFunction Type Expr
  | TypeSynonymInstance
  | OrphanInstance Ident (Qualified ProperName) [Type]
  | InvalidNewtype ProperName
  | InvalidInstanceHead Type
  | TransitiveExportError DeclarationRef [DeclarationRef]
  | ShadowedName Ident
  | ShadowedTypeVar String
  | UnusedTypeVar String
  | WildcardInferredType Type
  | MissingTypeDeclaration Ident Type
  | NotExhaustivePattern [[Binder]] Bool
  | OverlappingPattern [[Binder]] Bool
  | IncompleteExhaustivityCheck
  | ClassOperator ProperName Ident
  | MisleadingEmptyTypeImport ModuleName ProperName
  | ImportHidingModule ModuleName
  deriving Show

-- | Error message hints, providing more detailed information about failure.
data ErrorMessageHint
  = ErrorUnifyingTypes Type Type
  | ErrorInExpression Expr
  | ErrorInModule ModuleName
  | ErrorInInstance (Qualified ProperName) [Type]
  | ErrorInSubsumption Type Type
  | ErrorCheckingAccessor Expr String
  | ErrorCheckingType Expr Type
  | ErrorCheckingKind Type
  | ErrorInferringType Expr
  | ErrorInApplication Expr Type Expr
  | ErrorInDataConstructor ProperName
  | ErrorInTypeConstructor ProperName
  | ErrorInBindingGroup [Ident]
  | ErrorInDataBindingGroup
  | ErrorInTypeSynonym ProperName
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

instance UnificationError Type ErrorMessage where
  occursCheckFailed t = ErrorMessage [] $ InfiniteType t

instance UnificationError Kind ErrorMessage where
  occursCheckFailed k = ErrorMessage [] $ InfiniteKind k

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
  DuplicateModuleName{} -> "DuplicateModuleName"
  DuplicateClassExport{} -> "DuplicateClassExport"
  DuplicateValueExport{} -> "DuplicateValueExport"
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
  ShadowedName{} -> "ShadowedName"
  ShadowedTypeVar{} -> "ShadowedTypeVar"
  UnusedTypeVar{} -> "UnusedTypeVar"
  WildcardInferredType{} -> "WildcardInferredType"
  MissingTypeDeclaration{} -> "MissingTypeDeclaration"
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
unwrapErrorMessage (ErrorMessage _ se) = se

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
onTypesInErrorMessageM f (ErrorMessage hints simple) = ErrorMessage <$> traverse gHint hints <*> gSimple simple
  where
    gSimple (InfiniteType t) = InfiniteType <$> f t
    gSimple (TypesDoNotUnify t1 t2) = TypesDoNotUnify <$> f t1 <*> f t2
    gSimple (ConstrainedTypeUnified t1 t2) = ConstrainedTypeUnified <$> f t1 <*> f t2
    gSimple (ExprDoesNotHaveType e t) = ExprDoesNotHaveType e <$> f t
    gSimple (CannotApplyFunction t e) = CannotApplyFunction <$> f t <*> pure e
    gSimple (InvalidInstanceHead t) = InvalidInstanceHead <$> f t
    gSimple other = pure other
    gHint (ErrorInSubsumption t1 t2) = ErrorInSubsumption <$> f t1 <*> f t2
    gHint (ErrorUnifyingTypes t1 t2) = ErrorUnifyingTypes <$> f t1 <*> f t2
    gHint (ErrorCheckingType e t) = ErrorCheckingType e <$> f t
    gHint (ErrorCheckingKind t) = ErrorCheckingKind <$> f t
    gHint (ErrorInApplication e1 t1 e2) = ErrorInApplication e1 <$> f t1 <*> pure e2
    gHint other = pure other

-- |
-- Pretty print a single error, simplifying if necessary
--
prettyPrintSingleError :: Bool -> Level -> ErrorMessage -> State UnknownMap Box.Box
prettyPrintSingleError full level e = prettyPrintErrorMessage <$> onTypesInErrorMessageM replaceUnknowns (if full then e else simplifyErrorMessage e)
  where

  -- Pretty print an ErrorMessage
  prettyPrintErrorMessage :: ErrorMessage -> Box.Box
  prettyPrintErrorMessage (ErrorMessage hints simple) =
    paras $
      [ foldr renderHint (indent (renderSimpleErrorMessage simple)) hints
      , Box.moveDown 1 $ paras [ line $ "See " ++ wikiUri ++ " for more information, "
                               , line $ "or to contribute content related to this " ++ levelText ++ "."
                               ]
      ]
    where
    wikiUri :: String
    wikiUri = "https://github.com/purescript/purescript/wiki/Error-Code-" ++ errorCode e

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
    renderSimpleErrorMessage (ErrorParsingFFIModule path) =
      paras [ line "Unable to parse foreign module:"
            , indent . line $ path
            ]
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
    renderSimpleErrorMessage (ConflictingImport nm mn) =
      paras [ line $ "Cannot declare " ++ show nm ++ ", since another declaration of that name was imported from module " ++ runModuleName mn
            , line $ "Consider hiding " ++ show nm ++ " when importing " ++ runModuleName mn ++ ":"
            , indent . line $ "import " ++ runModuleName mn ++ " hiding (" ++ nm ++ ")"
            ]
    renderSimpleErrorMessage (ConflictingImports nm m1 m2) =
      line $ "Conflicting imports for " ++ nm ++ " from modules " ++ runModuleName m1 ++ " and " ++ runModuleName m2
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
    renderSimpleErrorMessage (CycleInDeclaration nm) =
      line $ "The value of " ++ showIdent nm ++ " is undefined here, so this reference is not allowed."
    renderSimpleErrorMessage (CycleInModules mns) =
      paras [ line $ "There is a cycle in module dependencies in these modules: "
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
                                          , indent $ prettyPrintValue expr
                                          ]) binding
    renderSimpleErrorMessage (TypesDoNotUnify t1 t2)
      = paras [ line "Could not match expected type"
              , indent $ typeAsBox t1
              , line "with actual type"
              , indent $ typeAsBox t2
              ]
    renderSimpleErrorMessage (KindsDoNotUnify k1 k2) =
      paras [ line "Could not match expected kind"
            , indent $ line $ prettyPrintKind k1
            , line "with actual kind"
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
            ]
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
                                             , indent $ prettyPrintValue expr'
                                             ]) expr
    renderSimpleErrorMessage (DuplicateTypeArgument name) =
      line $ "Type argument " ++ show name ++ " appears more than once."
    renderSimpleErrorMessage (DuplicateValueDeclaration nm) =
      line $ "Multiple value declarations exist for " ++ showIdent nm ++ "."
    renderSimpleErrorMessage (ArgListLengthsDiffer ident) =
      line $ "Argument list lengths differ in declaration " ++ showIdent ident
    renderSimpleErrorMessage (OverlappingArgNames ident) =
      line $ "Overlapping names in function/binder" ++ foldMap ((" in declaration" ++) . showIdent) ident
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
            , indent $ prettyPrintValue expr
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
            , indent $ prettyPrintValue arg
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
      paras $ line ("An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: ")
              : map (line . prettyPrintExport) ys
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
      paras [ line $ "'hiding' imports cannot be used to hide modules."
            , line $ "An attempt was made to hide the import of " ++ runModuleName name
            ]
    renderSimpleErrorMessage (WildcardInferredType ty) =
      paras [ line "Wildcard type definition has the inferred type "
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (MissingTypeDeclaration ident ty) =
      paras [ line $ "No type declaration was provided for the top-level declaration of " ++ showIdent ident ++ "."
            , line "It is good practice to provide type declarations as a form of documentation."
            , line $ "The inferred type of " ++ showIdent ident ++ " was:"
            , indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (NotExhaustivePattern bs b) =
      paras $ [ line "A case expression could not be determined to cover all inputs."
              , line "The following additional cases are required to cover all inputs:\n"
              , Box.hsep 1 Box.left (map (paras . map (line . prettyPrintBinderAtom)) (transpose bs))
              ] ++
              [ line "..." | not b ]
    renderSimpleErrorMessage (OverlappingPattern bs b) =
      paras $ [ line "A case expression contains unreachable cases:\n"
              , Box.hsep 1 Box.left (map (paras . map (line . prettyPrintBinderAtom)) (transpose bs))
              ] ++
              [ line "..." | not b ]
    renderSimpleErrorMessage IncompleteExhaustivityCheck =
      paras [ line "An exhaustivity check was abandoned due to too many possible cases."
            , line "You may want to decompose your data types into smaller types."
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
                                 , prettyPrintValue expr
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
    renderHint (ErrorInferringType expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the type of"
                                 , prettyPrintValue expr
                                 ]
            ]
    renderHint (ErrorCheckingType expr ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that expression"
                                 , prettyPrintValue expr
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has type"
                                                   , typeAsBox ty
                                                   ]
            ]
    renderHint (ErrorCheckingAccessor expr prop) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking type of property accessor"
                                 , prettyPrintValue (Accessor prop expr)
                                 ]
            ]
    renderHint (ErrorInApplication f t a) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while applying a function"
                                 , prettyPrintValue f
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "of type"
                                                   , typeAsBox t
                                                   ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "to argument"
                                                   , prettyPrintValue a
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

  levelText :: String
  levelText = case level of
    Error -> "error"
    Warning -> "warning"

  paras :: [Box.Box] -> Box.Box
  paras = Box.vcat Box.left

  -- Pretty print and export declaration
  prettyPrintExport :: DeclarationRef -> String
  prettyPrintExport (TypeRef pn _) = runProperName pn
  prettyPrintExport (ValueRef ident) = showIdent ident
  prettyPrintExport (TypeClassRef pn) = runProperName pn
  prettyPrintExport (TypeInstanceRef ident) = showIdent ident
  prettyPrintExport (ModuleRef name) = "module " ++ runModuleName name
  prettyPrintExport (PositionedDeclarationRef _ _ ref) = prettyPrintExport ref

  -- | Simplify an error message
  simplifyErrorMessage :: ErrorMessage -> ErrorMessage
  simplifyErrorMessage (ErrorMessage hints simple) = ErrorMessage (simplifyHints hints) simple
    where
    -- Take the last instance of each "hint category"
    simplifyHints :: [ErrorMessageHint] -> [ErrorMessageHint]
    simplifyHints = reverse . nubBy categoriesEqual . reverse

    -- Don't remove hints in the "other" category
    categoriesEqual :: ErrorMessageHint -> ErrorMessageHint -> Bool
    categoriesEqual x y =
      case (hintCategory x, hintCategory y) of
        (OtherHint, _) -> False
        (_, OtherHint) -> False
        (c1, c2) -> c1 == c2

  hintCategory :: ErrorMessageHint -> HintCategory
  hintCategory ErrorCheckingType{}  = ExprHint
  hintCategory ErrorInferringType{} = ExprHint
  hintCategory ErrorInExpression{}  = ExprHint
  hintCategory ErrorUnifyingTypes{} = CheckHint
  hintCategory ErrorInSubsumption{} = CheckHint
  hintCategory ErrorInApplication{} = CheckHint
  hintCategory PositionedError{}    = PositionHint
  hintCategory _                    = OtherHint

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
prettyPrintMultipleWarningsBox full = flip evalState M.empty . prettyPrintMultipleErrorsWith Warning "Warning found:" "Warning" full

-- | Pretty print errors as a Box
prettyPrintMultipleErrorsBox :: Bool -> MultipleErrors -> Box.Box
prettyPrintMultipleErrorsBox full = flip evalState M.empty . prettyPrintMultipleErrorsWith Error "Error found:" "Error" full

prettyPrintMultipleErrorsWith :: Level -> String -> String -> Bool -> MultipleErrors -> State UnknownMap Box.Box
prettyPrintMultipleErrorsWith level intro _ full  (MultipleErrors [e]) = do
  result <- prettyPrintSingleError full level e
  return $
    Box.vcat Box.left [ Box.text intro
                      , result
                      ]
prettyPrintMultipleErrorsWith level _ intro full  (MultipleErrors es) = do
  result <- forM es $ prettyPrintSingleError full level
  return $ Box.vsep 1 Box.left $ concat $ zipWith withIntro [1 :: Int ..] result
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
renderBox = unlines . map trimEnd . lines . Box.render
  where
  trimEnd = reverse . dropWhile (== ' ') . reverse

-- |
-- Interpret multiple errors and warnings in a monad supporting errors and warnings
--
interpretMultipleErrorsAndWarnings :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => (Either MultipleErrors a, MultipleErrors) -> m a
interpretMultipleErrorsAndWarnings (err, ws) = do
  tell ws
  either throwError return err

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
withPosition pos (ErrorMessage hints se) = ErrorMessage (PositionedError pos : hints) se

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
