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
-- Pretty print an ErrorMessage
--
prettyPrintErrorMessage :: ErrorMessage -> String
prettyPrintErrorMessage InvalidDoBind                   = "Bind statement cannot be the last statement in a do block"
prettyPrintErrorMessage InvalidDoLet                    = "Let statement cannot be the last statement in a do block"
prettyPrintErrorMessage CannotReorderOperators          = "Unable to reorder operators"
prettyPrintErrorMessage UnspecifiedSkolemScope          = "Skolem variable scope is unspecified"
prettyPrintErrorMessage OverlappingNamesInLet           = "Overlapping names in let binding."
prettyPrintErrorMessage (InfiniteType ty)               = "Infinite type detected: " ++ prettyPrintType ty
prettyPrintErrorMessage (InfiniteKind ki)               = "Infinite kind detected: " ++ prettyPrintKind ki
prettyPrintErrorMessage (MultipleFixities name)         = "Multiple fixity declarations for " ++ show name
prettyPrintErrorMessage (OrphanTypeDeclaration nm)      = "Orphan type declaration for " ++ show nm
prettyPrintErrorMessage (OrphanFixityDeclaration op)    = "Orphan fixity declaration for " ++ show op
prettyPrintErrorMessage (RedefinedModule name)          = "Module " ++ show name ++ " has been defined multiple times"
prettyPrintErrorMessage (RedefinedIdent name)           = "Name " ++ show name ++ " has been defined multiple times"
prettyPrintErrorMessage (UnknownModule mn)              = "Unknown module " ++ show mn
prettyPrintErrorMessage (UnknownType name)              = "Unknown type " ++ show name
prettyPrintErrorMessage (UnknownTypeClass name)         = "Unknown type class " ++ show name
prettyPrintErrorMessage (UnknownValue name)             = "Unknown value " ++ show name
prettyPrintErrorMessage (UnknownTypeConstructor name)   = "Unknown type constructor " ++ show name
prettyPrintErrorMessage (UnknownDataConstructor dc tc)  = "Unknown data constructor " ++ show dc ++ foldMap ((" for type constructor " ++) . show) tc
prettyPrintErrorMessage (ConflictingImport nm mn)       = "Declaration " ++ nm ++ " conflicts with import " ++ show mn 
prettyPrintErrorMessage (ConflictingImports nm m1 m2)   = "Conflicting imports for " ++ nm ++ " from modules " ++ show m1 ++ " and " ++ show m2
prettyPrintErrorMessage (ConflictingTypeDecls nm)       = "Conflicting type declarations for " ++ show nm
prettyPrintErrorMessage (ConflictingCtorDecls nm)       = "Conflicting data constructor declarations for " ++ show nm
prettyPrintErrorMessage (TypeConflictsWithClass nm)     = "Type " ++ show nm ++ " conflicts with type class declaration of the same name"
prettyPrintErrorMessage (CtorConflictsWithClass nm)     = "Data constructor " ++ show nm ++ " conflicts with type class declaration of the same name"
prettyPrintErrorMessage (ClassConflictsWithType nm)     = "Type class " ++ show nm ++ " conflicts with type declaration of the same name"
prettyPrintErrorMessage (ClassConflictsWithCtor nm)     = "Type class " ++ show nm ++ " conflicts with data constructor declaration of the same name"
prettyPrintErrorMessage (DuplicateClassExport nm)       = "Duplicate export declaration for type class " ++ show nm
prettyPrintErrorMessage (DuplicateValueExport nm)       = "Duplicate export declaration for value " ++ show nm
prettyPrintErrorMessage (CycleInDeclaration nm)         = "Cycle in declaration of " ++ show nm
prettyPrintErrorMessage (NotYetDefined names err)       = "The following are not yet defined here: " ++ unwords (map show names) ++ "\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (CycleInTypeSynonym pn)         = "Cycle in type synonym" ++ foldMap ((" " ++) . show) pn
prettyPrintErrorMessage (NameIsUndefined ident)         = show ident ++ " is undefined"
prettyPrintErrorMessage (NameNotInScope ident)          = show ident ++ " may not be defined in the current scope"
prettyPrintErrorMessage (UndefinedTypeVariable name)    = "Type variable " ++ show name ++ " is undefined"
prettyPrintErrorMessage (PartiallyAppliedSynonym name)  = "Partially applied type synonym " ++ show name
prettyPrintErrorMessage (EscapedSkolem binding)         = "Rigid/skolem type variable " ++ foldMap (("bound by " ++) . prettyPrintValue) binding ++ " has escaped."
prettyPrintErrorMessage (TypesDoNotUnify t1 t2)         = "Cannot unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2
prettyPrintErrorMessage (KindsDoNotUnify k1 k2)         = "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2
prettyPrintErrorMessage (ConstrainedTypeUnified t1 t2)  = "Cannot unify constrained type " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2
prettyPrintErrorMessage (OverlappingInstances nm ts ds) = unlines (("Overlapping instances found for " ++ show nm ++ " " ++ unwords (map prettyPrintType ts) ++ ":")
                                                                  : map prettyPrintDictionaryValue ds)
prettyPrintErrorMessage (NoInstanceFound nm ts)         = "No instance found for " ++ show nm ++ " " ++ unwords (map prettyPrintTypeAtom ts)
prettyPrintErrorMessage (DuplicateLabel l expr)         = "Duplicate label " ++ show l ++ " in row." ++ foldMap ((" Relevant expression: " ++) . prettyPrintValue) expr
prettyPrintErrorMessage (DuplicateTypeArgument name)    = "Duplicate type argument " ++ show name
prettyPrintErrorMessage (DuplicateValueDeclaration nm)  = "Duplicate value declaration for " ++ show nm
prettyPrintErrorMessage (ArgListLengthsDiffer ident)    = "Argument list lengths differ in declaration " ++ show ident
prettyPrintErrorMessage (OverlappingArgNames ident)     = "Overlapping names in function/binder" ++ foldMap ((" in declaration" ++) . show) ident
prettyPrintErrorMessage (MissingClassMember ident)      = "Member " ++ show ident ++ " has not been implemented"
prettyPrintErrorMessage (ExpectedType kind)             = "Expected type of kind *, was " ++ prettyPrintKind kind
prettyPrintErrorMessage (IncorrectConstructorArity nm)  = "Wrong number of arguments to constructor " ++ show nm
prettyPrintErrorMessage SubsumptionCheckFailed          = "Unable to check type subsumption"
prettyPrintErrorMessage (ExprDoesNotHaveType expr ty)   = "Expression " ++ prettyPrintValue expr ++ " does not have type " ++ prettyPrintType ty
prettyPrintErrorMessage (PropertyIsMissing prop row)    = "Row " ++ prettyPrintRow row ++ " lacks required property " ++ show prop
prettyPrintErrorMessage (CannotApplyFunction fn arg)    = "Cannot apply function of type " ++ prettyPrintType fn ++ " to argument " ++ prettyPrintValue arg
prettyPrintErrorMessage TypeSynonymInstance             = "Type synonym instances are disallowed"
prettyPrintErrorMessage InvalidNewtype                  = "Newtypes must define a single constructor with a single argument"
prettyPrintErrorMessage (InvalidInstanceHead ty)        = "Invalid type " ++ prettyPrintType ty ++ " in class instance head"
prettyPrintErrorMessage (TransitiveExportError x ys)    = "An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: " ++ intercalate ", " (map prettyPrintExport ys)
prettyPrintErrorMessage (ErrorUnifyingTypes t1 t2 err)  = "Error unifying type " ++ prettyPrintType t1 ++ " with type " ++ prettyPrintType t2 ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInExpression expr err)    = "Error in expression " ++ prettyPrintValue expr ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInModule mn err)          = "Error in module " ++ show mn ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInSubsumption t1 t2 err)  = "Error checking that type " ++ prettyPrintType t1 ++ " subsumes type " ++ prettyPrintType t2 ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInInstance name ts err)   = "Error in type class instance " ++ show name ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorCheckingKind ty err)      = "Error checking kind of type " ++ prettyPrintType ty ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInferringType expr err)   = "Error inferring type of value " ++ prettyPrintValue expr ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorCheckingType expr ty err) = "Error checking value " ++ prettyPrintValue expr ++ " has type " ++ prettyPrintType ty ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInApplication f t a err)  = "Error applying function " ++ prettyPrintValue f ++ " of type " ++ prettyPrintType t ++ " to argument " ++ prettyPrintValue a ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInDataConstructor nm err) = "Error in data constructor " ++ show nm ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInTypeConstructor nm err) = "Error in type constructor " ++ show nm ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInBindingGroup nms err)   = "Error in binding group " ++ intercalate ", " (map show nms) ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInDataBindingGroup err)   = "Error in data binding group:\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInTypeSynonym name err)   = "Error in type synonym " ++ show name ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInValueDeclaration n err) = "Error in value declaration " ++ show n ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInForeignImport nm err)   = "Error in foreign import " ++ show nm ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (PositionedError pos err)       = "Error at " ++ show pos ++ ":\n" ++ prettyPrintErrorMessage err

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
rethrowWithPosition pos = rethrow (onErrorMessages (PositionedError pos))

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
