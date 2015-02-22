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
import Data.List (intersperse)
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

-- |
-- A type of error messages
--
data ErrorMessage 
  = InfiniteType Type
  | InfiniteKind Kind
  | CannotReorderOperators
  | MultipleFixities Ident
  | OrphanTypeDeclaration Ident
  | RedefinedModule ModuleName
  | OverlappingNamesInLet
  | UnknownModule ModuleName
  | UnknownType (Qualified ProperName)
  | UnknownTypeClass (Qualified ProperName)
  | UnknownValue (Qualified Ident)
  | UnknownDataConstructor (Qualified ProperName) (Maybe (Qualified ProperName))
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
  | ErrorInExpression Expr ErrorMessage
  | ErrorInModule ModuleName ErrorMessage
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
prettyPrintErrorMessage OverlappingNamesInLet           = "Overlapping names in let binding."
prettyPrintErrorMessage (InfiniteType ty)               = "Infinite type detected: " ++ prettyPrintType ty
prettyPrintErrorMessage (InfiniteKind ki)               = "Infinite kind detected: " ++ prettyPrintKind ki
prettyPrintErrorMessage (MultipleFixities name)         = "Multiple fixity declarations for " ++ show name
prettyPrintErrorMessage (OrphanTypeDeclaration pn)      = "Orphan type declaration for: " ++ show pn
prettyPrintErrorMessage (RedefinedModule name)          = "Module " ++ show name ++ " has been defined multiple times"
prettyPrintErrorMessage (UnknownModule mn)              = "Unknown module: " ++ show mn
prettyPrintErrorMessage (UnknownType name)              = "Unknown type: " ++ show name
prettyPrintErrorMessage (UnknownTypeClass name)         = "Unknown type class: " ++ show name
prettyPrintErrorMessage (UnknownValue name)             = "Unknown value: " ++ show name
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
prettyPrintErrorMessage (ErrorInExpression expr err)    = "Error in expression " ++ prettyPrintValue expr ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (ErrorInModule mn err)          = "Error in module " ++ show mn ++ ":\n" ++ prettyPrintErrorMessage err
prettyPrintErrorMessage (PositionedError pos err)       = "Error at " ++ show pos ++ ":\n" ++ prettyPrintErrorMessage err

-- |
-- A stack trace for an error
--
newtype MultipleErrors = MultipleErrors 
  { runMultipleErrors :: [ErrorMessage] } deriving (Show, Monoid)

-- |
-- Simplify an error message
--
simplifyErrorMessage :: ErrorMessage -> ErrorMessage
simplifyErrorMessage = unwrap Nothing
  where
  unwrap :: Maybe SourceSpan -> ErrorMessage -> ErrorMessage
  unwrap pos (ErrorInExpression _ err) = unwrap pos err
  unwrap pos (ErrorInModule mn err) = ErrorInModule mn (unwrap pos err)
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
