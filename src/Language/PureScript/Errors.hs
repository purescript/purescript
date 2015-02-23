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
import Data.List (intersperse, intercalate)
import Data.Monoid
import Data.Foldable (fold, foldMap)

import Control.Monad.Except
import Control.Monad.Unify
import Control.Applicative ((<$>))

import Language.PureScript.AST
import Language.PureScript.Pretty
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries

-- |
-- A type of error messages
--
data ErrorMessage 
  = InfiniteType Type
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
  | NameIsUndefined Ident
  | NameNotInScope Ident
  | UndefinedTypeVariable ProperName
  | PartiallyAppliedSynonym (Qualified ProperName)
  | NotYetDefined [Ident] ErrorMessage
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
  | ErrorUnifyingTypes Type Type ErrorMessage
  | CannotApplyFunction Type Expr
  | TypeSynonymInstance
  | InvalidNewtype
  | InvalidInstanceHead Type
  | TransitiveExportError DeclarationRef [DeclarationRef]
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
  deriving (Show)
  
instance UnificationError Type ErrorMessage where
  occursCheckFailed = InfiniteType
  
instance UnificationError Kind ErrorMessage where
  occursCheckFailed = InfiniteKind

-- |
-- Get the error code for a particular error type
--
errorCode :: ErrorMessage -> String
errorCode (InfiniteType _)              = "InfiniteType"
errorCode (InfiniteKind _)              = "InfiniteKind"
errorCode CannotReorderOperators        = "CannotReorderOperators"
errorCode (MultipleFixities _)          = "MultipleFixities"
errorCode (OrphanTypeDeclaration _)     = "OrphanTypeDeclaration"
errorCode (OrphanFixityDeclaration _)   = "OrphanFixityDeclaration"
errorCode (RedefinedModule _)           = "RedefinedModule"
errorCode (RedefinedIdent _)            = "RedefinedIdent"
errorCode OverlappingNamesInLet         = "OverlappingNamesInLet"
errorCode (UnknownModule _)             = "UnknownModule"
errorCode (UnknownType _)               = "UnknownType"
errorCode (UnknownTypeClass _)          = "UnknownTypeClass"
errorCode (UnknownValue _)              = "UnknownValue"
errorCode (UnknownDataConstructor _ _)  = "UnknownDataConstructor"
errorCode (UnknownTypeConstructor _)    = "UnknownTypeConstructor"
errorCode (ConflictingImport _ _)       = "ConflictingImport"
errorCode (ConflictingImports _ _ _)    = "ConflictingImports"
errorCode (ConflictingTypeDecls _)      = "ConflictingTypeDecls"
errorCode (ConflictingCtorDecls _)      = "ConflictingCtorDecls"
errorCode (TypeConflictsWithClass _)    = "TypeConflictsWithClass"
errorCode (CtorConflictsWithClass _)    = "CtorConflictsWithClass"
errorCode (ClassConflictsWithType _)    = "ClassConflictsWithType"
errorCode (ClassConflictsWithCtor _)    = "ClassConflictsWithCtor"
errorCode (DuplicateClassExport _)      = "DuplicateClassExport"
errorCode (DuplicateValueExport _)      = "DuplicateValueExport"
errorCode (DuplicateTypeArgument _)     = "DuplicateTypeArgument"
errorCode InvalidDoBind                 = "InvalidDoBind"
errorCode InvalidDoLet                  = "InvalidDoLet"
errorCode (CycleInDeclaration _)        = "CycleInDeclaration"
errorCode (CycleInTypeSynonym _)        = "CycleInTypeSynonym"
errorCode (NameIsUndefined _)           = "NameIsUndefined"
errorCode (NameNotInScope _)            = "NameNotInScope"
errorCode (UndefinedTypeVariable _)     = "UndefinedTypeVariable"
errorCode (PartiallyAppliedSynonym _)   = "PartiallyAppliedSynonym"
errorCode (EscapedSkolem _)             = "EscapedSkolem"
errorCode UnspecifiedSkolemScope        = "UnspecifiedSkolemScope"
errorCode (TypesDoNotUnify _ _)         = "TypesDoNotUnify"
errorCode (KindsDoNotUnify _ _)         = "KindsDoNotUnify"
errorCode (ConstrainedTypeUnified _ _)  = "ConstrainedTypeUnified"
errorCode (OverlappingInstances _ _ _)  = "OverlappingInstances"
errorCode (NoInstanceFound _ _)         = "NoInstanceFound"
errorCode (DuplicateLabel _ _)          = "DuplicateLabel"
errorCode (DuplicateValueDeclaration _) = "DuplicateValueDeclaration"
errorCode (ArgListLengthsDiffer _)      = "ArgListLengthsDiffer"
errorCode (OverlappingArgNames _)       = "OverlappingArgNames"
errorCode (MissingClassMember _)        = "MissingClassMember"
errorCode (ExpectedType _)              = "ExpectedType"
errorCode (IncorrectConstructorArity _) = "IncorrectConstructorArity"
errorCode SubsumptionCheckFailed        = "SubsumptionCheckFailed"
errorCode (ExprDoesNotHaveType _ _)     = "ExprDoesNotHaveType"
errorCode (PropertyIsMissing _ _)       = "PropertyIsMissing"
errorCode (ErrorUnifyingTypes _ _ _)    = "ErrorUnifyingTypes"
errorCode (CannotApplyFunction _ _)     = "CannotApplyFunction"
errorCode TypeSynonymInstance           = "TypeSynonymInstance"
errorCode InvalidNewtype                = "InvalidNewtype"
errorCode (InvalidInstanceHead _)       = "InvalidInstanceHead"
errorCode (TransitiveExportError _ _)   = "TransitiveExportError"
errorCode (NotYetDefined _ e)           = errorCode e
errorCode (ErrorInExpression _ e)       = errorCode e
errorCode (ErrorInModule _ e)           = errorCode e
errorCode (ErrorInInstance _ _ e)       = errorCode e
errorCode (ErrorInSubsumption _ _ e)    = errorCode e
errorCode (ErrorCheckingType _ _ e)     = errorCode e
errorCode (ErrorCheckingKind _ e)       = errorCode e
errorCode (ErrorInferringType _ e)      = errorCode e
errorCode (ErrorInApplication _ _ _ e)  = errorCode e
errorCode (ErrorInDataConstructor _ e)  = errorCode e
errorCode (ErrorInTypeConstructor _ e)  = errorCode e
errorCode (ErrorInBindingGroup _ e)     = errorCode e
errorCode (ErrorInDataBindingGroup e)   = errorCode e
errorCode (ErrorInTypeSynonym _ e)      = errorCode e
errorCode (ErrorInValueDeclaration _ e) = errorCode e
errorCode (ErrorInForeignImport _ e)    = errorCode e
errorCode (PositionedError _ e)         = errorCode e

-- |
-- Pretty print an ErrorMessage
--
prettyPrintErrorMessage :: ErrorMessage -> String
prettyPrintErrorMessage e =
  go e ++ "\n\nSee " ++ wikiUri ++ " for more information, or to contribute content related to this error."
  where
  wikiUri :: String
  wikiUri = "https://github.com/purescript/purescript/wiki/Error-Code-" ++ errorCode e
      
  go :: ErrorMessage -> String
  go InvalidDoBind                   = "Bind statement cannot be the last statement in a do block"
  go InvalidDoLet                    = "Let statement cannot be the last statement in a do block"
  go CannotReorderOperators          = "Unable to reorder operators"
  go UnspecifiedSkolemScope          = "Skolem variable scope is unspecified"
  go OverlappingNamesInLet           = "Overlapping names in let binding."
  go (InfiniteType ty)               = "Infinite type detected: " ++ prettyPrintType ty
  go (InfiniteKind ki)               = "Infinite kind detected: " ++ prettyPrintKind ki
  go (MultipleFixities name)         = "Multiple fixity declarations for " ++ show name
  go (OrphanTypeDeclaration nm)      = "Orphan type declaration for " ++ show nm
  go (OrphanFixityDeclaration op)    = "Orphan fixity declaration for " ++ show op
  go (RedefinedModule name)          = "Module " ++ show name ++ " has been defined multiple times"
  go (RedefinedIdent name)           = "Name " ++ show name ++ " has been defined multiple times"
  go (UnknownModule mn)              = "Unknown module " ++ show mn
  go (UnknownType name)              = "Unknown type " ++ show name
  go (UnknownTypeClass name)         = "Unknown type class " ++ show name
  go (UnknownValue name)             = "Unknown value " ++ show name
  go (UnknownTypeConstructor name)   = "Unknown type constructor " ++ show name
  go (UnknownDataConstructor dc tc)  = "Unknown data constructor " ++ show dc ++ foldMap ((" for type constructor " ++) . show) tc
  go (ConflictingImport nm mn)       = "Declaration " ++ nm ++ " conflicts with import " ++ show mn 
  go (ConflictingImports nm m1 m2)   = "Conflicting imports for " ++ nm ++ " from modules " ++ show m1 ++ " and " ++ show m2
  go (ConflictingTypeDecls nm)       = "Conflicting type declarations for " ++ show nm
  go (ConflictingCtorDecls nm)       = "Conflicting data constructor declarations for " ++ show nm
  go (TypeConflictsWithClass nm)     = "Type " ++ show nm ++ " conflicts with type class declaration of the same name"
  go (CtorConflictsWithClass nm)     = "Data constructor " ++ show nm ++ " conflicts with type class declaration of the same name"
  go (ClassConflictsWithType nm)     = "Type class " ++ show nm ++ " conflicts with type declaration of the same name"
  go (ClassConflictsWithCtor nm)     = "Type class " ++ show nm ++ " conflicts with data constructor declaration of the same name"
  go (DuplicateClassExport nm)       = "Duplicate export declaration for type class " ++ show nm
  go (DuplicateValueExport nm)       = "Duplicate export declaration for value " ++ show nm
  go (CycleInDeclaration nm)         = "Cycle in declaration of " ++ show nm
  go (NotYetDefined names err)       = "The following are not yet defined here: " ++ unwords (map show names) ++ "\n" ++ go err
  go (CycleInTypeSynonym pn)         = "Cycle in type synonym" ++ foldMap ((" " ++) . show) pn
  go (NameIsUndefined ident)         = show ident ++ " is undefined"
  go (NameNotInScope ident)          = show ident ++ " may not be defined in the current scope"
  go (UndefinedTypeVariable name)    = "Type variable " ++ show name ++ " is undefined"
  go (PartiallyAppliedSynonym name)  = "Partially applied type synonym " ++ show name
  go (EscapedSkolem binding)         = "Rigid/skolem type variable " ++ foldMap (("bound by " ++) . prettyPrintValue) binding ++ " has escaped."
  go (TypesDoNotUnify t1 t2)         = "Cannot unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2
  go (KindsDoNotUnify k1 k2)         = "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2
  go (ConstrainedTypeUnified t1 t2)  = "Cannot unify constrained type " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2
  go (OverlappingInstances nm ts ds) = unlines ( ("Overlapping instances found for " ++ show nm ++ " " ++ unwords (map prettyPrintType ts) ++ ":")
                                               : map prettyPrintDictionaryValue ds)
  go (NoInstanceFound nm ts)         = "No instance found for " ++ show nm ++ " " ++ unwords (map prettyPrintTypeAtom ts)
  go (DuplicateLabel l expr)         = "Duplicate label " ++ show l ++ " in row." ++ foldMap ((" Relevant expression: " ++) . prettyPrintValue) expr
  go (DuplicateTypeArgument name)    = "Duplicate type argument " ++ show name
  go (DuplicateValueDeclaration nm)  = "Duplicate value declaration for " ++ show nm
  go (ArgListLengthsDiffer ident)    = "Argument list lengths differ in declaration " ++ show ident
  go (OverlappingArgNames ident)     = "Overlapping names in function/binder" ++ foldMap ((" in declaration" ++) . show) ident
  go (MissingClassMember ident)      = "Member " ++ show ident ++ " has not been implemented"
  go (ExpectedType kind)             = "Expected type of kind *, was " ++ prettyPrintKind kind
  go (IncorrectConstructorArity nm)  = "Wrong number of arguments to constructor " ++ show nm
  go SubsumptionCheckFailed          = "Unable to check type subsumption"
  go (ExprDoesNotHaveType expr ty)   = "Expression " ++ prettyPrintValue expr ++ " does not have type " ++ prettyPrintType ty
  go (PropertyIsMissing prop row)    = "Row " ++ prettyPrintRow row ++ " lacks required property " ++ show prop
  go (CannotApplyFunction fn arg)    = "Cannot apply function of type " ++ prettyPrintType fn ++ " to argument " ++ prettyPrintValue arg
  go TypeSynonymInstance             = "Type synonym instances are disallowed"
  go InvalidNewtype                  = "Newtypes must define a single constructor with a single argument"
  go (InvalidInstanceHead ty)        = "Invalid type " ++ prettyPrintType ty ++ " in class instance head"
  go (TransitiveExportError x ys)    = "An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: " ++ intercalate ", " (map prettyPrintExport ys)
  go (ErrorUnifyingTypes t1 t2 err)  = "Error unifying type " ++ prettyPrintType t1 ++ " with type " ++ prettyPrintType t2 ++ ":\n" ++ go err
  go (ErrorInExpression expr err)    = "Error in expression " ++ prettyPrintValue expr ++ ":\n" ++ go err
  go (ErrorInModule mn err)          = "Error in module " ++ show mn ++ ":\n" ++ go err
  go (ErrorInSubsumption t1 t2 err)  = "Error checking that type " ++ prettyPrintType t1 ++ " subsumes type " ++ prettyPrintType t2 ++ ":\n" ++ go err
  go (ErrorInInstance name ts err)   = "Error in type class instance " ++ show name ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":\n" ++ go err
  go (ErrorCheckingKind ty err)      = "Error checking kind of type " ++ prettyPrintType ty ++ ":\n" ++ go err
  go (ErrorInferringType expr err)   = "Error inferring type of value " ++ prettyPrintValue expr ++ ":\n" ++ go err
  go (ErrorCheckingType expr ty err) = "Error checking value " ++ prettyPrintValue expr ++ " has type " ++ prettyPrintType ty ++ ":\n" ++ go err
  go (ErrorInApplication f t a err)  = "Error applying function " ++ prettyPrintValue f ++ " of type " ++ prettyPrintType t ++ " to argument " ++ prettyPrintValue a ++ ":\n" ++ go err
  go (ErrorInDataConstructor nm err) = "Error in data constructor " ++ show nm ++ ":\n" ++ go err
  go (ErrorInTypeConstructor nm err) = "Error in type constructor " ++ show nm ++ ":\n" ++ go err
  go (ErrorInBindingGroup nms err)   = "Error in binding group " ++ intercalate ", " (map show nms) ++ ":\n" ++ go err
  go (ErrorInDataBindingGroup err)   = "Error in data binding group:\n" ++ go err
  go (ErrorInTypeSynonym name err)   = "Error in type synonym " ++ show name ++ ":\n" ++ go err
  go (ErrorInValueDeclaration n err) = "Error in value declaration " ++ show n ++ ":\n" ++ go err
  go (ErrorInForeignImport nm err)   = "Error in foreign import " ++ show nm ++ ":\n" ++ go err
  go (PositionedError pos err)       = "Error at " ++ show pos ++ ":\n" ++ go err

-- |
-- Render a DictionaryValue fit for human consumption in error messages
--
prettyPrintDictionaryValue :: DictionaryValue -> String
prettyPrintDictionaryValue = unlines . indented 0
  where
  indented n (LocalDictionaryValue _)           = [spaces n ++ "Dictionary in scope"]
  indented n (GlobalDictionaryValue nm)         = [spaces n ++ show nm]
  indented n (DependentDictionaryValue nm args) = (spaces n ++ show nm ++ " via") : concatMap (indented (n + 2)) args
  indented n (SubclassDictionaryValue sup nm _) = (spaces n ++ show nm ++ " via superclass") : indented (n + 2) sup

  spaces n = replicate n ' ' ++ "- "
  
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
-- A stack trace for an error
--
newtype MultipleErrors = MultipleErrors 
  { runMultipleErrors :: [ErrorMessage] } deriving (Show, Monoid)
  
instance UnificationError Type MultipleErrors where
  occursCheckFailed = errorMessage . occursCheckFailed
  
instance UnificationError Kind MultipleErrors where
  occursCheckFailed = errorMessage . occursCheckFailed

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
-- Create an error set from a single error message
--
errorMessage :: ErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [err]

-- |
-- Lift a function on ErrorMessage to a function on MultipleErrors
--
onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f = MultipleErrors . map f . runMultipleErrors

-- |
-- Pretty print a single error, simplifying if necessary
--
prettyPrintSingleError :: Bool -> ErrorMessage -> String
prettyPrintSingleError full e = prettyPrintErrorMessage (if full then e else simplifyErrorMessage e)

-- |
-- Pretty print multiple errors
--
prettyPrintMultipleErrors :: Bool -> MultipleErrors -> String
prettyPrintMultipleErrors full  (MultipleErrors [e]) = 
  prettyPrintSingleError full e 
prettyPrintMultipleErrors full  (MultipleErrors es) =
  unlines $ intersperse "" $ "Multiple errors:" : map (prettyPrintSingleError full) es

-- |
-- Interpret multiple errors in a monad supporting errors
--
interpretMultipleErrors :: (MonadError String m) => Bool -> Either MultipleErrors a -> m a
interpretMultipleErrors printFullStack = either (throwError . prettyPrintMultipleErrors printFullStack) return

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
