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

import qualified Text.PrettyPrint.Boxes as Box

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
errorCode (CannotApplyFunction _ _)     = "CannotApplyFunction"
errorCode TypeSynonymInstance           = "TypeSynonymInstance"
errorCode InvalidNewtype                = "InvalidNewtype"
errorCode (InvalidInstanceHead _)       = "InvalidInstanceHead"
errorCode (TransitiveExportError _ _)   = "TransitiveExportError"
errorCode (NotYetDefined _ e)           = errorCode e
errorCode (ErrorUnifyingTypes _ _ e)    = errorCode e
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
-- A stack trace for an error
--
newtype MultipleErrors = MultipleErrors 
  { runMultipleErrors :: [ErrorMessage] } deriving (Show, Monoid)
  
instance UnificationError Type MultipleErrors where
  occursCheckFailed = errorMessage . occursCheckFailed
  
instance UnificationError Kind MultipleErrors where
  occursCheckFailed = errorMessage . occursCheckFailed

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
prettyPrintSingleError :: Bool -> ErrorMessage -> Box.Box
prettyPrintSingleError full e = prettyPrintErrorMessage (if full then e else simplifyErrorMessage e)
  where
  -- |
  -- Pretty print an ErrorMessage
  --
  prettyPrintErrorMessage :: ErrorMessage -> Box.Box
  prettyPrintErrorMessage em =
    paras
      [ go em
      , line ("See " ++ wikiUri ++ " for more information, or to contribute content related to this error.") 
      ]
    where
    wikiUri :: String
    wikiUri = "https://github.com/purescript/purescript/wiki/Error-Code-" ++ errorCode e
      
    go :: ErrorMessage -> Box.Box
    go InvalidDoBind                   = line "Bind statement cannot be the last statement in a do block"
    go InvalidDoLet                    = line "Let statement cannot be the last statement in a do block"
    go CannotReorderOperators          = line "Unable to reorder operators"
    go UnspecifiedSkolemScope          = line "Skolem variable scope is unspecified"
    go OverlappingNamesInLet           = line "Overlapping names in let binding."
    go (InfiniteType ty)               = paras [ line "Infinite type detected: "
                                               , indent $ line $ prettyPrintType ty
                                               ]
    go (InfiniteKind ki)               = paras [ line "Infinite kind detected: "
                                               , indent $ line $ prettyPrintKind ki
                                               ]
    go (MultipleFixities name)         = line $ "Multiple fixity declarations for " ++ show name
    go (OrphanTypeDeclaration nm)      = line $ "Orphan type declaration for " ++ show nm
    go (OrphanFixityDeclaration op)    = line $ "Orphan fixity declaration for " ++ show op
    go (RedefinedModule name)          = line $ "Module " ++ show name ++ " has been defined multiple times"
    go (RedefinedIdent name)           = line $ "Name " ++ show name ++ " has been defined multiple times"
    go (UnknownModule mn)              = line $ "Unknown module " ++ show mn
    go (UnknownType name)              = line $ "Unknown type " ++ show name
    go (UnknownTypeClass name)         = line $ "Unknown type class " ++ show name
    go (UnknownValue name)             = line $ "Unknown value " ++ show name
    go (UnknownTypeConstructor name)   = line $ "Unknown type constructor " ++ show name
    go (UnknownDataConstructor dc tc)  = line $ "Unknown data constructor " ++ show dc ++ foldMap ((" for type constructor " ++) . show) tc
    go (ConflictingImport nm mn)       = line $ "Declaration " ++ nm ++ " conflicts with import " ++ show mn 
    go (ConflictingImports nm m1 m2)   = line $ "Conflicting imports for " ++ nm ++ " from modules " ++ show m1 ++ " and " ++ show m2
    go (ConflictingTypeDecls nm)       = line $ "Conflicting type declarations for " ++ show nm
    go (ConflictingCtorDecls nm)       = line $ "Conflicting data constructor declarations for " ++ show nm
    go (TypeConflictsWithClass nm)     = line $ "Type " ++ show nm ++ " conflicts with type class declaration of the same name"
    go (CtorConflictsWithClass nm)     = line $ "Data constructor " ++ show nm ++ " conflicts with type class declaration of the same name"
    go (ClassConflictsWithType nm)     = line $ "Type class " ++ show nm ++ " conflicts with type declaration of the same name"
    go (ClassConflictsWithCtor nm)     = line $ "Type class " ++ show nm ++ " conflicts with data constructor declaration of the same name"
    go (DuplicateClassExport nm)       = line $ "Duplicate export declaration for type class " ++ show nm
    go (DuplicateValueExport nm)       = line $ "Duplicate export declaration for value " ++ show nm
    go (CycleInDeclaration nm)         = line $ "Cycle in declaration of " ++ show nm
    go (NotYetDefined names err)       = paras [ line $ "The following are not yet defined here: " ++ intercalate ", " (map show names) ++ ":"
                                               , indent $ go err
                                               ]
    go (CycleInTypeSynonym pn)         = line $ "Cycle in type synonym" ++ foldMap ((" " ++) . show) pn
    go (NameIsUndefined ident)         = line $ show ident ++ " is undefined"
    go (NameNotInScope ident)          = line $ show ident ++ " may not be defined in the current scope"
    go (UndefinedTypeVariable name)    = line $ "Type variable " ++ show name ++ " is undefined"
    go (PartiallyAppliedSynonym name)  = line $ "Partially applied type synonym " ++ show name
    go (EscapedSkolem binding)         = paras $ [ line "Rigid/skolem type variable has escaped." ]
                                                 <> foldMap (\expr -> [ line "Relevant expression: "
                                                                      , indent $ line $ prettyPrintValue expr 
                                                                      ]) binding
    go (TypesDoNotUnify t1 t2)         = paras [ line "Cannot unify type"
                                               , indent $ line $ prettyPrintType t1
                                               , line "with type"
                                               , indent $ line $ prettyPrintType t2
                                               ]
    go (KindsDoNotUnify k1 k2)         = paras [ line "Cannot unify kind"
                                               , indent $ line $ prettyPrintKind k1
                                               , line "with kind"
                                               , indent $ line $ prettyPrintKind k2
                                               ]
    go (ConstrainedTypeUnified t1 t2)  = paras [ line "Cannot unify constrained type"
                                               , indent $ line $ prettyPrintType t1
                                               , line "with type"
                                               , indent $ line $ prettyPrintType t2
                                               ]
    go (OverlappingInstances nm ts ds) = paras [ line $ "Overlapping instances found for " ++ show nm ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":"
                                               , paras $ map prettyPrintDictionaryValue ds
                                               ]
    go (NoInstanceFound nm ts)         = line $ "No instance found for " ++ show nm ++ " " ++ unwords (map prettyPrintTypeAtom ts)
    go (DuplicateLabel l expr)         = paras $ [ line $ "Duplicate label " ++ show l ++ " in row." ]
                                                 <> foldMap (\expr' -> [ line "Relevant expression: "
                                                                       , indent $ line $ prettyPrintValue expr' 
                                                                       ]) expr
    go (DuplicateTypeArgument name)    = line $ "Duplicate type argument " ++ show name
    go (DuplicateValueDeclaration nm)  = line $ "Duplicate value declaration for " ++ show nm
    go (ArgListLengthsDiffer ident)    = line $ "Argument list lengths differ in declaration " ++ show ident
    go (OverlappingArgNames ident)     = line $ "Overlapping names in function/binder" ++ foldMap ((" in declaration" ++) . show) ident
    go (MissingClassMember ident)      = line $ "Member " ++ show ident ++ " has not been implemented"
    go (ExpectedType kind)             = line $ "Expected type of kind *, was " ++ prettyPrintKind kind
    go (IncorrectConstructorArity nm)  = line $ "Wrong number of arguments to constructor " ++ show nm
    go SubsumptionCheckFailed          = line $ "Unable to check type subsumption"
    go (ExprDoesNotHaveType expr ty)   = paras [ line "Expression"
                                               , indent $ line $ prettyPrintValue expr
                                               , line "does not have type"
                                               , indent $ line $ prettyPrintType ty
                                               ]
    go (PropertyIsMissing prop row)    = line $ "Row " ++ prettyPrintRow row ++ " lacks required property " ++ show prop
    go (CannotApplyFunction fn arg)    = paras [ line "Cannot apply function of type"
                                               , indent $ line $ prettyPrintType fn
                                               , line "to argument"
                                               , indent $ line $ prettyPrintValue arg
                                               ]
    go TypeSynonymInstance             = line "Type synonym instances are disallowed"
    go InvalidNewtype                  = line "Newtypes must define a single constructor with a single argument"
    go (InvalidInstanceHead ty)        = paras [ line "Invalid type in class instance head:"
                                               , indent $ line $ prettyPrintType ty
                                               ]
    go (TransitiveExportError x ys)    = paras $ (line $ "An export for " ++ prettyPrintExport x ++ " requires the following to also be exported: ")
                                                 : map (line . prettyPrintExport) ys
    go (ErrorUnifyingTypes t1 t2 err)  = paras [ line "Error unifying type "
                                               , indent $ line $ prettyPrintType t1
                                               , line "with type"
                                               , indent $ line $ prettyPrintType t2
                                               , go err
                                               ]
    go (ErrorInExpression expr err)    = paras [ line "Error in expression:"
                                               , indent $ line $ prettyPrintValue expr
                                               , go err
                                               ]
    go (ErrorInModule mn err)          = paras [ line $ "Error in module " ++ show mn ++ ":"
                                               , go err
                                               ]
    go (ErrorInSubsumption t1 t2 err)  = paras [ line "Error checking that type "
                                               , indent $ line $ prettyPrintType t1
                                               , line "subsumes type"
                                               , indent $ line $ prettyPrintType t2
                                               , go err
                                               ]
    go (ErrorInInstance name ts err)   = paras [ line $ "Error in type class instance " ++ show name ++ " " ++ unwords (map prettyPrintTypeAtom ts) ++ ":"
                                               , go err
                                               ]
    go (ErrorCheckingKind ty err)      = paras [ line "Error checking kind of type "
                                               , indent $ line $ prettyPrintType ty
                                               , go err
                                               ]
    go (ErrorInferringType expr err)   = paras [ line "Error inferring type of value "
                                               , indent $ line $ prettyPrintValue expr
                                               , go err
                                               ]
    go (ErrorCheckingType expr ty err) = paras [ line "Error checking that value "
                                               , indent $ line $ prettyPrintValue expr
                                               , line "has type"
                                               , indent $ line $ prettyPrintType ty
                                               , go err
                                               ]
    go (ErrorInApplication f t a err)  = paras [ line "Error applying function"
                                               , indent $ line $ prettyPrintValue f
                                               , line "of type"
                                               , indent $ line $ prettyPrintType t
                                               , line "to argument"
                                               , indent $ line $ prettyPrintValue a
                                               , go err
                                               ]
    go (ErrorInDataConstructor nm err) = paras [ line $ "Error in data constructor " ++ show nm ++ ":"
                                               , go err
                                               ]
    go (ErrorInTypeConstructor nm err) = paras [ line $ "Error in type constructor " ++ show nm ++ ":"
                                               , go err
                                               ]
    go (ErrorInBindingGroup nms err)   = paras [ line $ "Error in binding group " ++ intercalate ", " (map show nms) ++ ":"
                                               , go err
                                               ]
    go (ErrorInDataBindingGroup err)   = paras [ line $ "Error in data binding group:"
                                               , go err
                                               ]
    go (ErrorInTypeSynonym name err)   = paras [ line $ "Error in type synonym " ++ show name ++ ":"
                                               , go err
                                               ]
    go (ErrorInValueDeclaration n err) = paras [ line $ "Error in value declaration " ++ show n ++ ":"
                                               , go err
                                               ]
    go (ErrorInForeignImport nm err)   = paras [ line $ "Error in foreign import " ++ show nm ++ ":"
                                               , go err
                                               ]
    go (PositionedError pos err)       = paras [ line $ "Error at " ++ show pos ++ ":"
                                               , indent $ go err
                                               ]

  line :: String -> Box.Box
  line = Box.text

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
prettyPrintMultipleErrors full  (MultipleErrors [e]) = renderBox $
  prettyPrintSingleError full e 
prettyPrintMultipleErrors full  (MultipleErrors es) = renderBox $
  Box.vcat Box.left [ Box.text "Multiple errors:"
                    , Box.vsep 1 Box.left $ map (Box.moveRight 2 . prettyPrintSingleError full) es
                    ]

renderBox :: Box.Box -> String
renderBox = unlines . map trimEnd . lines . Box.render
  where
  trimEnd = reverse . dropWhile (== ' ') . reverse

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
