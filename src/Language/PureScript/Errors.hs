-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Error
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
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

module Language.PureScript.Errors where

import Data.Either (lefts, rights)
import Data.List (intercalate)
import Data.Function (on)
import Data.Monoid
import Data.Foldable (fold, foldMap)

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Unify
import Control.Monad.Writer
import Control.Monad.Error.Class (MonadError(..))
import Control.Applicative ((<$>), (<*>), Applicative, pure)
import Control.Monad.Trans.State.Lazy
import Control.Arrow(first)

import Language.PureScript.AST
import Language.PureScript.Environment (isObject)
import Language.PureScript.Pretty
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries

import qualified Text.PrettyPrint.Boxes as Box

import qualified Text.Parsec as P

-- |
-- A type of error messages
--
data SimpleErrorMessage
  = ErrorParsingExterns P.ParseError
  | ErrorParsingFFIModule FilePath
  | ErrorParsingPrelude P.ParseError
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
  | RedefinedModule ModuleName
  | RedefinedIdent Ident
  | OverlappingNamesInLet
  | UnknownModule ModuleName
  | UnknownType (Qualified ProperName)
  | UnknownTypeClass (Qualified ProperName)
  | UnknownValue (Qualified Ident)
  | UnknownDataConstructor (Qualified ProperName) (Maybe (Qualified ProperName))
  | UnknownTypeConstructor (Qualified ProperName)
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
  | OverlappingInstances (Qualified ProperName) [Type] [DictionaryValue]
  | NoInstanceFound (Qualified ProperName) [Type]
  | DuplicateLabel String (Maybe Expr)
  | DuplicateValueDeclaration Ident
  | ArgListLengthsDiffer Ident
  | OverlappingArgNames (Maybe Ident)
  | MissingClassMember Ident
  | ExpectedType Kind
  | IncorrectConstructorArity (Qualified ProperName)
  | SubsumptionCheckFailed
  | ExprDoesNotHaveType Expr Type
  | PropertyIsMissing String Type
  | CannotApplyFunction Type Expr
  | TypeSynonymInstance
  | InvalidNewtype
  | InvalidInstanceHead Type
  | TransitiveExportError DeclarationRef [DeclarationRef]
  | ShadowedName Ident
  | WildcardInferredType Type
  | PreludeNotPresent
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
  (ErrorParsingExterns _)       -> "ErrorParsingExterns"
  (ErrorParsingFFIModule _)     -> "ErrorParsingFFIModule"
  (ErrorParsingPrelude _)       -> "ErrorParsingPrelude"
  (ErrorParsingModule _)        -> "ErrorParsingModule"
  MissingFFIModule{}            -> "MissingFFIModule"
  MultipleFFIModules{}          -> "MultipleFFIModules"
  UnnecessaryFFIModule{}        -> "UnnecessaryFFIModule"
  (InvalidExternsFile _)        -> "InvalidExternsFile"
  (CannotGetFileInfo _)         -> "CannotGetFileInfo"
  (CannotReadFile _)            -> "CannotReadFile"
  (CannotWriteFile _)           -> "CannotWriteFile"
  (InfiniteType _)              -> "InfiniteType"
  (InfiniteKind _)              -> "InfiniteKind"
  CannotReorderOperators        -> "CannotReorderOperators"
  (MultipleFixities _)          -> "MultipleFixities"
  (OrphanTypeDeclaration _)     -> "OrphanTypeDeclaration"
  (OrphanFixityDeclaration _)   -> "OrphanFixityDeclaration"
  (RedefinedModule _)           -> "RedefinedModule"
  (RedefinedIdent _)            -> "RedefinedIdent"
  OverlappingNamesInLet         -> "OverlappingNamesInLet"
  (UnknownModule _)             -> "UnknownModule"
  (UnknownType _)               -> "UnknownType"
  (UnknownTypeClass _)          -> "UnknownTypeClass"
  (UnknownValue _)              -> "UnknownValue"
  (UnknownDataConstructor _ _)  -> "UnknownDataConstructor"
  (UnknownTypeConstructor _)    -> "UnknownTypeConstructor"
  (ConflictingImport _ _)       -> "ConflictingImport"
  (ConflictingImports _ _ _)    -> "ConflictingImports"
  (ConflictingTypeDecls _)      -> "ConflictingTypeDecls"
  (ConflictingCtorDecls _)      -> "ConflictingCtorDecls"
  (TypeConflictsWithClass _)    -> "TypeConflictsWithClass"
  (CtorConflictsWithClass _)    -> "CtorConflictsWithClass"
  (ClassConflictsWithType _)    -> "ClassConflictsWithType"
  (ClassConflictsWithCtor _)    -> "ClassConflictsWithCtor"
  (DuplicateClassExport _)      -> "DuplicateClassExport"
  (DuplicateValueExport _)      -> "DuplicateValueExport"
  (DuplicateTypeArgument _)     -> "DuplicateTypeArgument"
  InvalidDoBind                 -> "InvalidDoBind"
  InvalidDoLet                  -> "InvalidDoLet"
  (CycleInDeclaration _)        -> "CycleInDeclaration"
  (CycleInTypeSynonym _)        -> "CycleInTypeSynonym"
  (CycleInModules _)            -> "CycleInModules"
  (NameIsUndefined _)           -> "NameIsUndefined"
  (NameNotInScope _)            -> "NameNotInScope"
  (UndefinedTypeVariable _)     -> "UndefinedTypeVariable"
  (PartiallyAppliedSynonym _)   -> "PartiallyAppliedSynonym"
  (EscapedSkolem _)             -> "EscapedSkolem"
  UnspecifiedSkolemScope        -> "UnspecifiedSkolemScope"
  (TypesDoNotUnify _ _)         -> "TypesDoNotUnify"
  (KindsDoNotUnify _ _)         -> "KindsDoNotUnify"
  (ConstrainedTypeUnified _ _)  -> "ConstrainedTypeUnified"
  (OverlappingInstances _ _ _)  -> "OverlappingInstances"
  (NoInstanceFound _ _)         -> "NoInstanceFound"
  (DuplicateLabel _ _)          -> "DuplicateLabel"
  (DuplicateValueDeclaration _) -> "DuplicateValueDeclaration"
  (ArgListLengthsDiffer _)      -> "ArgListLengthsDiffer"
  (OverlappingArgNames _)       -> "OverlappingArgNames"
  (MissingClassMember _)        -> "MissingClassMember"
  (ExpectedType _)              -> "ExpectedType"
  (IncorrectConstructorArity _) -> "IncorrectConstructorArity"
  SubsumptionCheckFailed        -> "SubsumptionCheckFailed"
  (ExprDoesNotHaveType _ _)     -> "ExprDoesNotHaveType"
  (PropertyIsMissing _ _)       -> "PropertyIsMissing"
  (CannotApplyFunction _ _)     -> "CannotApplyFunction"
  TypeSynonymInstance           -> "TypeSynonymInstance"
  InvalidNewtype                -> "InvalidNewtype"
  (InvalidInstanceHead _)       -> "InvalidInstanceHead"
  (TransitiveExportError _ _)   -> "TransitiveExportError"
  (ShadowedName _)              -> "ShadowedName"
  (WildcardInferredType _)      -> "WildcardInferredType"
  PreludeNotPresent             -> "PreludeNotPresent"

-- |
-- A stack trace for an error
--
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage] } deriving (Show, Monoid)

instance UnificationError Type MultipleErrors where
  occursCheckFailed = occursCheckFailed

instance UnificationError Kind MultipleErrors where
  occursCheckFailed = occursCheckFailed

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
data LabelType = TypeLabel | SkolemLabel String deriving (Show, Eq, Ord)

-- | A map from rigid type variable name/unknown variable pairs to new variables.
type UnknownMap = M.Map (LabelType, Unknown) Unknown

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
    g (ErrorInForeignImport x e) = ErrorInForeignImport x <$> (g e)
    g (PositionedError x e) = PositionedError x <$> (g e)
    g (SimpleErrorWrapper sem) = SimpleErrorWrapper <$> gSimple sem

-- |
-- Pretty print a single error, simplifying if necessary
--
prettyPrintSingleError :: Bool -> ErrorMessage -> State UnknownMap Box.Box
prettyPrintSingleError full e = prettyPrintErrorMessage <$> onTypesInErrorMessageM replaceUnknowns (if full then e else simplifyErrorMessage e)
 where
  -- |
  -- Pretty print an ErrorMessage
  --
  prettyPrintErrorMessage :: ErrorMessage -> Box.Box
  prettyPrintErrorMessage em =
    paras $
      go em:suggestions em ++
      [line $ "See " ++ wikiUri ++ " for more information, or to contribute content related to this error."]
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
      paras [ line "Error parsing externs files: "
            , indent . line . show $ err
            ]
    goSimple (ErrorParsingFFIModule path) =
      paras [ line "Unable to parse module from FFI file: "
            , indent . line $ path
            ]
    goSimple (ErrorParsingModule err) =
      paras [ line "Unable to parse module: "
            , indent . line . show $ err
            ]
    goSimple (MissingFFIModule mn) =
      line $ "Missing FFI implementations for module " ++ show mn
    goSimple (UnnecessaryFFIModule mn path) =
      paras [ line $ "Unnecessary FFI implementations have been provided for module " ++ show mn ++ ": "
            , indent . line $ path
            ]
    goSimple (MultipleFFIModules mn paths) =
      paras $ [ line $ "Multiple FFI implementations have been provided for module " ++ show mn ++ ": " ]
            ++ map (indent . line) paths
    goSimple (ErrorParsingPrelude err) =
      paras [ line "Error parsing prelude: "
            , indent . line . show $ err
            ]
    goSimple (InvalidExternsFile path) =
      paras [ line "Externs file is invalid: "
            , indent . line $ path
            ]
    goSimple InvalidDoBind =
      line "Bind statement cannot be the last statement in a do block"
    goSimple InvalidDoLet =
      line "Let statement cannot be the last statement in a do block"
    goSimple CannotReorderOperators =
      line "Unable to reorder operators"
    goSimple UnspecifiedSkolemScope =
      line "Skolem variable scope is unspecified"
    goSimple OverlappingNamesInLet =
      line "Overlapping names in let binding."
    goSimple (InfiniteType ty) =
      paras [ line "Infinite type detected: "
            , indent $ line $ prettyPrintType ty
            ]
    goSimple (InfiniteKind ki) =
      paras [ line "Infinite kind detected: "
            , indent $ line $ prettyPrintKind ki
            ]
    goSimple (MultipleFixities name) =
      line $ "Multiple fixity declarations for " ++ show name
    goSimple (OrphanTypeDeclaration nm) =
      line $ "Orphan type declaration for " ++ show nm
    goSimple (OrphanFixityDeclaration op) =
      line $ "Orphan fixity declaration for " ++ show op
    goSimple (RedefinedModule name) =
      line $ "Module " ++ show name ++ " has been defined multiple times"
    goSimple (RedefinedIdent name) =
      line $ "Name " ++ show name ++ " has been defined multiple times"
    goSimple (UnknownModule mn) =
      line $ "Unknown module " ++ show mn
    goSimple (UnknownType name) =
      line $ "Unknown type " ++ show name
    goSimple (UnknownTypeClass name) =
      line $ "Unknown type class " ++ show name
    goSimple (UnknownValue name) =
      line $ "Unknown value " ++ show name
    goSimple (UnknownTypeConstructor name) =
      line $ "Unknown type constructor " ++ show name
    goSimple (UnknownDataConstructor dc tc) =
      line $ "Unknown data constructor " ++ show dc ++ foldMap ((" for type constructor " ++) . show) tc
    goSimple (ConflictingImport nm mn) =
      line $ "Cannot declare `" ++ nm ++ "` since another declaration of that name was imported from `" ++ show mn ++ "`"
    goSimple (ConflictingImports nm m1 m2) =
      line $ "Conflicting imports for " ++ nm ++ " from modules " ++ show m1 ++ " and " ++ show m2
    goSimple (ConflictingTypeDecls nm) =
      line $ "Conflicting type declarations for " ++ show nm
    goSimple (ConflictingCtorDecls nm) =
      line $ "Conflicting data constructor declarations for " ++ show nm
    goSimple (TypeConflictsWithClass nm) =
      line $ "Type " ++ show nm ++ " conflicts with type class declaration of the same name"
    goSimple (CtorConflictsWithClass nm) =
      line $ "Data constructor " ++ show nm ++ " conflicts with type class declaration of the same name"
    goSimple (ClassConflictsWithType nm) =
      line $ "Type class " ++ show nm ++ " conflicts with type declaration of the same name"
    goSimple (ClassConflictsWithCtor nm) =
      line $ "Type class " ++ show nm ++ " conflicts with data constructor declaration of the same name"
    goSimple (DuplicateClassExport nm) =
      line $ "Duplicate export declaration for type class " ++ show nm
    goSimple (DuplicateValueExport nm) =
      line $ "Duplicate export declaration for value " ++ show nm
    goSimple (CycleInDeclaration nm) =
      line $ "Cycle in declaration of " ++ show nm
    goSimple (CycleInModules mns) =
      line $ "Cycle in module dependencies: " ++ intercalate ", " (map show mns)
    goSimple (CycleInTypeSynonym pn) =
      line $ "Cycle in type synonym" ++ foldMap ((" " ++) . show) pn
    goSimple (NameIsUndefined ident) =
      line $ show ident ++ " is undefined"
    goSimple (NameNotInScope ident) =
      line $ show ident ++ " may not be defined in the current scope"
    goSimple (UndefinedTypeVariable name) =
      line $ "Type variable " ++ show name ++ " is undefined"
    goSimple (PartiallyAppliedSynonym name) =
      line $ "Partially applied type synonym " ++ show name
    goSimple (EscapedSkolem binding) =
      paras $ [ line "Rigid/skolem type variable has escaped." ]
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
    goSimple (OverlappingInstances nm ts ds) =
      paras [ line $ "Overlapping instances found for " ++ show nm ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":"
            , paras $ map prettyPrintDictionaryValue ds
            ]
    goSimple (NoInstanceFound nm ts) =
      line $ "No instance found for " ++ show nm ++ " " ++ unwords (map prettyPrintTypeAtom ts)
    goSimple (DuplicateLabel l expr) =
      paras $ [ line $ "Duplicate label " ++ show l ++ " in row." ]
                       <> foldMap (\expr' -> [ line "Relevant expression: "
                                             , indent $ line $ prettyPrintValue expr'
                                             ]) expr
    goSimple (DuplicateTypeArgument name) =
      line $ "Duplicate type argument " ++ show name
    goSimple (DuplicateValueDeclaration nm) =
      line $ "Duplicate value declaration for " ++ show nm
    goSimple (ArgListLengthsDiffer ident) =
      line $ "Argument list lengths differ in declaration " ++ show ident
    goSimple (OverlappingArgNames ident) =
      line $ "Overlapping names in function/binder" ++ foldMap ((" in declaration" ++) . show) ident
    goSimple (MissingClassMember ident) =
      line $ "Member " ++ show ident ++ " has not been implemented"
    goSimple (ExpectedType kind) =
      line $ "Expected type of kind *, was " ++ prettyPrintKind kind
    goSimple (IncorrectConstructorArity nm) =
      line $ "Wrong number of arguments to constructor " ++ show nm
    goSimple SubsumptionCheckFailed = line $ "Unable to check type subsumption"
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
    goSimple InvalidNewtype =
      line "Newtypes must define a single constructor with a single argument"
    goSimple (InvalidInstanceHead ty) =
      paras [ line "Invalid type in class instance head:"
            , indent $ line $ prettyPrintType ty
            ]
    goSimple (TransitiveExportError x ys) =
      paras $ (line $ "An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: ")
              : map (line . prettyPrintExport) ys
    goSimple (ShadowedName nm) =
      line $ "Name '" ++ show nm ++ "' was shadowed."
    goSimple (WildcardInferredType ty) =
      line $ "The wildcard type definition has the inferred type " ++ prettyPrintType ty
    goSimple PreludeNotPresent =
      paras [ line $ "There is no Prelude module loaded, and the --no-prelude option was not specified."
            , line $ "You probably need to install the Prelude and other dependencies using Bower."
            ]
    go (NotYetDefined names err) =
      paras [ line $ "The following are not yet defined here: " ++ intercalate ", " (map show names) ++ ":"
            , indent $ go err
            ]
    go (ErrorUnifyingTypes t1 t2 err) =
      paras [ line "Error unifying type "
            , indent $ line $ prettyPrintType t1
            , line "with type"
            , indent $ line $ prettyPrintType t2
            , go err
            ]
    go (ErrorInExpression expr err) =
      paras [ line "Error in expression:"
            , indent $ line $ prettyPrintValue expr
            , go err
            ]
    go (ErrorInModule mn err) =
      paras [ line $ "Error in module " ++ show mn ++ ":"
            , go err
            ]
    go (ErrorInSubsumption t1 t2 err) =
      paras [ line "Error checking that type "
            , indent $ line $ prettyPrintType t1
            , line "subsumes type"
            , indent $ line $ prettyPrintType t2
            , go err
            ]
    go (ErrorInInstance name ts err) =
      paras [ line $ "Error in type class instance " ++ show name ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":"
            , go err
            ]
    go (ErrorCheckingKind ty err) =
      paras [ line "Error checking kind of type "
            , indent $ line $ prettyPrintType ty
            , go err
            ]
    go (ErrorInferringType expr err) =
      paras [ line "Error inferring type of value "
            , indent $ line $ prettyPrintValue expr
            , go err
            ]
    go (ErrorCheckingType expr ty err) =
      paras [ line "Error checking that value "
            , indent $ line $ prettyPrintValue expr
            , line "has type"
            , indent $ line $ prettyPrintType ty
            , go err
            ]
    go (ErrorInApplication f t a err) =
      paras [ line "Error applying function"
            , indent $ line $ prettyPrintValue f
            , line "of type"
            , indent $ line $ prettyPrintType t
            , line "to argument"
            , indent $ line $ prettyPrintValue a
            , go err
            ]
    go (ErrorInDataConstructor nm err) =
      paras [ line $ "Error in data constructor " ++ show nm ++ ":"
            , go err
            ]
    go (ErrorInTypeConstructor nm err) =
      paras [ line $ "Error in type constructor " ++ show nm ++ ":"
            , go err
            ]
    go (ErrorInBindingGroup nms err) =
      paras [ line $ "Error in binding group " ++ intercalate ", " (map show nms) ++ ":"
            , go err
            ]
    go (ErrorInDataBindingGroup err) =
      paras [ line $ "Error in data binding group:"
            , go err
            ]
    go (ErrorInTypeSynonym name err) =
      paras [ line $ "Error in type synonym " ++ show name ++ ":"
            , go err
            ]
    go (ErrorInValueDeclaration n err) =
      paras [ line $ "Error in value declaration " ++ show n ++ ":"
            , go err
            ]
    go (ErrorInForeignImport nm err) =
      paras [ line $ "Error in foreign import " ++ show nm ++ ":"
            , go err
            ]
    go (PositionedError srcSpan err) =
      paras [ line $ "Error at " ++ displaySourceSpan srcSpan ++ ":"
            , indent $ go err
            ]
    go (SimpleErrorWrapper sem) = goSimple sem

  line :: String -> Box.Box
  line = Box.text

  suggestions :: ErrorMessage -> [Box.Box]
  suggestions = suggestions' . unwrapErrorMessage
    where
    suggestions' (ConflictingImport nm im) = [ line $ "Possible fix: hide `" ++ nm ++ "` when importing `" ++ show im ++ "`:"
                                             , indent . line $ "import " ++ show im ++ " hiding (" ++ nm ++ ")"
                                             ]
    suggestions' (TypesDoNotUnify t1 t2)
      | any isObject [t1, t2] = [line "Note that function composition in PureScript is defined using `(<<<)`"]
      | otherwise             = []
    suggestions' _ = []

  paras :: [Box.Box] -> Box.Box
  paras = Box.vcat Box.left

  indent :: Box.Box -> Box.Box
  indent = Box.moveRight 2

  -- |
  -- Render a DictionaryValue fit for human consumption in error messages
  --
  prettyPrintDictionaryValue :: DictionaryValue -> Box.Box
  prettyPrintDictionaryValue (LocalDictionaryValue _)           = line "Dictionary in scope"
  prettyPrintDictionaryValue (GlobalDictionaryValue nm)         = line (show nm)
  prettyPrintDictionaryValue (DependentDictionaryValue nm args) = paras [ line $ (show nm) ++ " via"
                                                                        , indent $ paras $ map prettyPrintDictionaryValue args
                                                                        ]
  prettyPrintDictionaryValue (SubclassDictionaryValue sup nm _) = paras [ line $ (show nm) ++ " via superclass"
                                                                        , indent $ prettyPrintDictionaryValue sup
                                                                        ]

  -- |
  -- Pretty print and export declaration
  --
  prettyPrintExport :: DeclarationRef -> String
  prettyPrintExport (TypeRef pn _) = show pn
  prettyPrintExport (ValueRef ident) = show ident
  prettyPrintExport (TypeClassRef pn) = show pn
  prettyPrintExport (TypeInstanceRef ident) = show ident
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
prettyPrintMultipleErrors full = flip evalState M.empty . prettyPrintMultipleErrorsWith "Error:" "Multiple errors:" full

-- |
-- Pretty print multiple warnings
--
prettyPrintMultipleWarnings :: Bool -> MultipleErrors ->  String
prettyPrintMultipleWarnings full = flip evalState M.empty . prettyPrintMultipleErrorsWith "Warning:" "Multiple warnings:" full

prettyPrintMultipleErrorsWith :: String -> String -> Bool -> MultipleErrors -> State UnknownMap String
prettyPrintMultipleErrorsWith intro _ full  (MultipleErrors [e]) = do
  result <- prettyPrintSingleError full e
  return $ renderBox $
    Box.vcat Box.left [ Box.text intro
                      , result
                      ]
prettyPrintMultipleErrorsWith _ intro full  (MultipleErrors es) = do
  result <- forM es $ (liftM $ Box.moveRight 2) . prettyPrintSingleError full
  return $ renderBox $
    Box.vcat Box.left [ Box.text intro
                      , Box.vsep 1 Box.left result
                      ]

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

-- |
-- Rethrow an error with source position information
--
rethrowWithPosition :: (MonadError MultipleErrors m) => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (onErrorMessages withPosition)
  where
  withPosition :: ErrorMessage -> ErrorMessage
  withPosition (PositionedError _ err) = withPosition err
  withPosition err = PositionedError pos err

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
