{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Errors
  ( module Language.PureScript.AST
  , module Language.PureScript.Errors
  ) where

import           Prelude.Compat
import           Protolude (ordNub)

import           Control.Arrow ((&&&))
import           Control.Monad
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Writer
import           Data.Char (isSpace)
import           Data.Either (partitionEithers)
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity(..))
import           Data.List (transpose, nubBy, sort, partition, dropWhileEnd)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (maybeToList, fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text (Text)
import           Language.PureScript.AST
import qualified Language.PureScript.Bundle as Bundle
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Label (Label(..))
import           Language.PureScript.Names
import           Language.PureScript.Pretty
import           Language.PureScript.Pretty.Common (endWith)
import           Language.PureScript.PSString (decodeStringWithReplacement)
import           Language.PureScript.Traversals
import           Language.PureScript.Types
import qualified Language.PureScript.Publish.BoxesHelpers as BoxHelpers
import qualified System.Console.ANSI as ANSI
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE
import           Text.Parsec.Error (Message(..))
import qualified Text.PrettyPrint.Boxes as Box

newtype ErrorSuggestion = ErrorSuggestion Text

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

-- | Get the error code for a particular error type
errorCode :: ErrorMessage -> Text
errorCode em = case unwrapErrorMessage em of
  ModuleNotFound{} -> "ModuleNotFound"
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
  MultipleValueOpFixities{} -> "MultipleValueOpFixities"
  MultipleTypeOpFixities{} -> "MultipleTypeOpFixities"
  OrphanTypeDeclaration{} -> "OrphanTypeDeclaration"
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
  AmbiguousTypeVariables{} -> "AmbiguousTypeVariables"
  UnknownClass{} -> "UnknownClass"
  PossiblyInfiniteInstance{} -> "PossiblyInfiniteInstance"
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
  OverlappingPattern{} -> "OverlappingPattern"
  IncompleteExhaustivityCheck{} -> "IncompleteExhaustivityCheck"
  MisleadingEmptyTypeImport{} -> "MisleadingEmptyTypeImport"
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

-- | A stack trace for an error
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage]
  } deriving (Show, Monoid)

-- | Check whether a collection of errors is empty or not.
nonEmpty :: MultipleErrors -> Bool
nonEmpty = not . null . runMultipleErrors

-- | Create an error set from a single simple error message
errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [ErrorMessage [] err]

-- | Create an error set from a single simple error message and source annotation
errorMessage' :: SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage' ss err = MultipleErrors [ErrorMessage [PositionedError ss] err]

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

replaceUnknowns :: Type -> State TypeMap Type
replaceUnknowns = everywhereOnTypesM replaceTypes where
  replaceTypes :: Type -> State TypeMap Type
  replaceTypes (TUnknown u) = do
    m <- get
    case M.lookup u (umUnknownMap m) of
      Nothing -> do
        let u' = umNextIndex m
        put $ m { umUnknownMap = M.insert u u' (umUnknownMap m), umNextIndex = u' + 1 }
        return (TUnknown u')
      Just u' -> return (TUnknown u')
  replaceTypes (Skolem name s sko ss) = do
    m <- get
    case M.lookup s (umSkolemMap m) of
      Nothing -> do
        let s' = umNextIndex m
        put $ m { umSkolemMap = M.insert s (T.unpack name, s', ss) (umSkolemMap m), umNextIndex = s' + 1 }
        return (Skolem name s' sko ss)
      Just (_, s', _) -> return (Skolem name s' sko ss)
  replaceTypes other = return other

onTypesInErrorMessage :: (Type -> Type) -> ErrorMessage -> ErrorMessage
onTypesInErrorMessage f = runIdentity . onTypesInErrorMessageM (Identity . f)

onTypesInErrorMessageM :: Applicative m => (Type -> m Type) -> ErrorMessage -> m ErrorMessage
onTypesInErrorMessageM f (ErrorMessage hints simple) = ErrorMessage <$> traverse gHint hints <*> gSimple simple
  where
  gSimple (InfiniteType t) = InfiniteType <$> f t
  gSimple (TypesDoNotUnify t1 t2) = TypesDoNotUnify <$> f t1 <*> f t2
  gSimple (ConstrainedTypeUnified t1 t2) = ConstrainedTypeUnified <$> f t1 <*> f t2
  gSimple (ExprDoesNotHaveType e t) = ExprDoesNotHaveType e <$> f t
  gSimple (InvalidInstanceHead t) = InvalidInstanceHead <$> f t
  gSimple (NoInstanceFound con) = NoInstanceFound <$> overConstraintArgs (traverse f) con
  gSimple (AmbiguousTypeVariables t con) = AmbiguousTypeVariables <$> f t <*> pure con
  gSimple (OverlappingInstances cl ts insts) = OverlappingInstances cl <$> traverse f ts <*> pure insts
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
  gSimple (HoleInferredType name ty ctx env) = HoleInferredType name <$> f ty <*> traverse (sndM f) ctx  <*> onTypeSearchTypesM f env
  gSimple (MissingTypeDeclaration nm ty) = MissingTypeDeclaration nm <$> f ty
  gSimple (CannotGeneralizeRecursiveFunction nm ty) = CannotGeneralizeRecursiveFunction nm <$> f ty
  gSimple other = pure other

  gHint (ErrorInSubsumption t1 t2) = ErrorInSubsumption <$> f t1 <*> f t2
  gHint (ErrorUnifyingTypes t1 t2) = ErrorUnifyingTypes <$> f t1 <*> f t2
  gHint (ErrorCheckingType e t) = ErrorCheckingType e <$> f t
  gHint (ErrorCheckingKind t) = ErrorCheckingKind <$> f t
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
      HidingImport mn refs -> suggest $ importSuggestion mn refs Nothing
      MissingTypeDeclaration ident ty -> suggest $ showIdent ident <> " :: " <> T.pack (prettyPrintSuggestedType ty)
      WildcardInferredType ty _ -> suggest $ T.pack (prettyPrintSuggestedType ty)
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
  getSpan (unwrapErrorMessage e) <$> errorSpan e
  where
    startOnly SourceSpan{spanName, spanStart} = SourceSpan {spanName, spanStart, spanEnd = spanStart}

    getSpan simple ss =
      case simple of
        MissingTypeDeclaration{} -> startOnly ss
        _ -> ss

showSuggestion :: SimpleErrorMessage -> Text
showSuggestion suggestion = case errorSuggestion suggestion of
  Just (ErrorSuggestion x) -> x
  _ -> ""

ansiColor :: (ANSI.ColorIntensity, ANSI.Color) -> String
ansiColor (intesity, color) =
   ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intesity color]

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


-- | Default color intesity and color for code
defaultCodeColor :: (ANSI.ColorIntensity, ANSI.Color)
defaultCodeColor = (ANSI.Dull, ANSI.Yellow)

-- | `prettyPrintSingleError` Options
data PPEOptions = PPEOptions
  { ppeCodeColor         :: Maybe (ANSI.ColorIntensity, ANSI.Color) -- ^ Color code with this color... or not
  , ppeFull              :: Bool -- ^ Should write a full error message?
  , ppeLevel             :: Level -- ^ Should this report an error or a warning?
  , ppeShowDocs          :: Bool -- ^ Should show a link to error message's doc page?
  , ppeRelativeDirectory :: FilePath -- ^ FilePath to which the errors are relative
  }

-- | Default options for PPEOptions
defaultPPEOptions :: PPEOptions
defaultPPEOptions = PPEOptions
  { ppeCodeColor         = Just defaultCodeColor
  , ppeFull              = False
  , ppeLevel             = Error
  , ppeShowDocs          = True
  , ppeRelativeDirectory = mempty
  }

-- | Pretty print a single error, simplifying if necessary
prettyPrintSingleError :: PPEOptions -> ErrorMessage -> Box.Box
prettyPrintSingleError (PPEOptions codeColor full level showDocs relPath) e = flip evalState defaultUnknownMap $ do
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
    renderSimpleErrorMessage (ModuleNotFound mn) =
      paras [ line $ "Module " <> markCode (runModuleName mn) <> " was not found."
            , line "Make sure the source file exists, and that it has been provided as an input to the compiler."
            ]
    renderSimpleErrorMessage (CannotGetFileInfo path) =
      paras [ line "Unable to read file info: "
            , indent . lineS $ path
            ]
    renderSimpleErrorMessage (CannotReadFile path) =
      paras [ line "Unable to read file: "
            , indent . lineS $ path
            ]
    renderSimpleErrorMessage (CannotWriteFile path) =
      paras [ line "Unable to write file: "
            , indent . lineS $ path
            ]
    renderSimpleErrorMessage (ErrorParsingFFIModule path extra) =
      paras $ [ line "Unable to parse foreign module:"
              , indent . lineS $ path
              ] ++
              map (indent . lineS) (concatMap Bundle.printErrorMessage (maybeToList extra))
    renderSimpleErrorMessage (ErrorParsingModule err) =
      paras [ line "Unable to parse module: "
            , prettyPrintParseError err
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
    renderSimpleErrorMessage (MultipleFFIModules mn paths) =
      paras [ line $ "Multiple foreign module implementations have been provided for module " <> markCode (runModuleName mn) <> ": "
            , indent . paras $ map lineS paths
            ]
    renderSimpleErrorMessage InvalidDoBind =
      line "The last statement in a 'do' block must be an expression, but this block ends with a binder."
    renderSimpleErrorMessage InvalidDoLet =
      line "The last statement in a 'do' block must be an expression, but this block ends with a let binding."
    renderSimpleErrorMessage OverlappingNamesInLet =
      line "The same name was used more than once in a let binding."
    renderSimpleErrorMessage (InfiniteType ty) =
      paras [ line "An infinite type was inferred for an expression: "
            , markCodeBox $ indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (InfiniteKind ki) =
      paras [ line "An infinite kind was inferred for a type: "
            , indent $ line $ markCode $ prettyPrintKind ki
            ]
    renderSimpleErrorMessage (MultipleValueOpFixities op) =
      line $ "There are multiple fixity/precedence declarations for operator " <> markCode (showOp op)
    renderSimpleErrorMessage (MultipleTypeOpFixities op) =
      line $ "There are multiple fixity/precedence declarations for type operator " <> markCode (showOp op)
    renderSimpleErrorMessage (OrphanTypeDeclaration nm) =
      line $ "The type declaration for " <> markCode (showIdent nm) <> " should be followed by its definition."
    renderSimpleErrorMessage (RedefinedIdent name) =
      line $ "The value " <> markCode (showIdent name) <> " has been defined multiple times"
    renderSimpleErrorMessage (UnknownName name@(Qualified Nothing (IdentName (Ident i)))) | i `elem` [ C.bind, C.discard ] =
      line $ "Unknown " <> printName name <> ". You're probably using do-notation, which the compiler replaces with calls to the " <> markCode i <> " function. Please import " <> markCode i <> " from module " <> markCode "Prelude"
    renderSimpleErrorMessage (UnknownName name) =
      line $ "Unknown " <> printName name
    renderSimpleErrorMessage (UnknownImport mn name) =
      paras [ line $ "Cannot import " <> printName (Qualified Nothing name) <> " from module " <> markCode (runModuleName mn)
            , line "It either does not exist or the module does not export it."
            ]
    renderSimpleErrorMessage (UnknownImportDataConstructor mn tcon dcon) =
      line $ "Module " <> runModuleName mn <> " does not export data constructor " <> markCode (runProperName dcon) <> " for type " <> markCode (runProperName tcon)
    renderSimpleErrorMessage (UnknownExport name) =
      line $ "Cannot export unknown " <> printName (Qualified Nothing name)
    renderSimpleErrorMessage (UnknownExportDataConstructor tcon dcon) =
      line $ "Cannot export data constructor " <> markCode (runProperName dcon) <> " for type " <> markCode (runProperName tcon) <> ", as it has not been declared."
    renderSimpleErrorMessage (ScopeConflict nm ms) =
      paras [ line $ "Conflicting definitions are in scope for " <> printName (Qualified Nothing nm) <> " from the following modules:"
            , indent $ paras $ map (line . markCode . runModuleName) ms
            ]
    renderSimpleErrorMessage (ScopeShadowing nm exmn ms) =
      paras [ line $ "Shadowed definitions are in scope for " <> printName (Qualified Nothing nm) <> " from the following open imports:"
            , indent $ paras $ map (line . markCode . ("import " <>) . runModuleName) ms
            , line $ "These will be ignored and the " <> case exmn of
                Just exmn' -> "declaration from " <> markCode (runModuleName exmn') <> " will be used."
                Nothing -> "local declaration will be used."
            ]
    renderSimpleErrorMessage (DeclConflict new existing) =
      line $ "Declaration for " <> printName (Qualified Nothing new) <> " conflicts with an existing " <> nameType existing <> " of the same name."
    renderSimpleErrorMessage (ExportConflict new existing) =
      line $ "Export for " <> printName new <> " conflicts with " <> runName existing
    renderSimpleErrorMessage (DuplicateModule mn ss) =
      paras [ line ("Module " <> markCode (runModuleName mn) <> " has been defined multiple times:")
            , indent . paras $ map (line . displaySourceSpan relPath) ss
            ]
    renderSimpleErrorMessage (CycleInDeclaration nm) =
      line $ "The value of " <> markCode (showIdent nm) <> " is undefined here, so this reference is not allowed."
    renderSimpleErrorMessage (CycleInModules mns) =
      paras [ line "There is a cycle in module dependencies in these modules: "
            , indent $ paras (map (line . markCode . runModuleName) mns)
            ]
    renderSimpleErrorMessage (CycleInTypeSynonym name) =
      paras [ line $ case name of
                       Just pn -> "A cycle appears in the definition of type synonym " <> markCode (runProperName pn)
                       Nothing -> "A cycle appears in a set of type synonym definitions."
            , line "Cycles are disallowed because they can lead to loops in the type checker."
            , line "Consider using a 'newtype' instead."
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
            , markCodeBox $ indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (EscapedSkolem name (Just srcSpan) ty) =
      paras [ line $ "The type variable " <> markCode name <> ", bound at"
            , indent $ line $ displaySourceSpan relPath srcSpan
            , line "has escaped its scope, appearing in the type"
            , markCodeBox $ indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (TypesDoNotUnify u1 u2)
      = let (sorted1, sorted2) = sortRows u1 u2

            sortRows :: Type -> Type -> (Type, Type)
            sortRows r1@RCons{} r2@RCons{} = sortRows' (rowToList r1) (rowToList r2)
            sortRows t1 t2 = (t1, t2)

            -- Put the common labels last
            sortRows' :: ([(Label, Type)], Type) -> ([(Label, Type)], Type) -> (Type, Type)
            sortRows' (s1, r1) (s2, r2) =
                  let (common1, unique1) = partition (flip elem s2) s1
                      (common2, unique2) = partition (flip elem s1) s2
                  in ( rowFromList (sort unique1 ++ sort common1, r1)
                     , rowFromList (sort unique2 ++ sort common2, r2)
                     )
        in paras [ line "Could not match type"
                 , markCodeBox $ indent $ typeAsBox sorted1
                 , line "with type"
                 , markCodeBox $ indent $ typeAsBox sorted2
                 ]

    renderSimpleErrorMessage (KindsDoNotUnify k1 k2) =
      paras [ line "Could not match kind"
            , indent $ line $ markCode $ prettyPrintKind k1
            , line "with kind"
            , indent $ line $ markCode $ prettyPrintKind k2
            ]
    renderSimpleErrorMessage (ConstrainedTypeUnified t1 t2) =
      paras [ line "Could not match constrained type"
            , markCodeBox $ indent $ typeAsBox t1
            , line "with type"
            , markCodeBox $ indent $ typeAsBox t2
            ]
    renderSimpleErrorMessage (OverlappingInstances nm ts (d : ds)) =
      paras [ line "Overlapping type class instances found for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line "The following instances were found:"
            , indent $ paras (line (showQualified showIdent d <> " (chosen)") : map (line . showQualified showIdent) ds)
            , line "Overlapping type class instances can lead to different behavior based on the order of module imports, and for that reason are not recommended."
            , line "They may be disallowed completely in a future version of the compiler."
            ]
    renderSimpleErrorMessage OverlappingInstances{} = internalError "OverlappingInstances: empty instance list"
    renderSimpleErrorMessage (UnknownClass nm) =
      paras [ line "No type class instance was found for class"
            , markCodeBox $ indent $ line (showQualified runProperName nm)
            , line "because the class was not in scope. Perhaps it was not exported."
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint C.Fail [ ty ] _)) | Just box <- toTypelevelString ty =
      paras [ line "A custom type error occurred while solving type class constraints:"
            , indent box
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint C.Partial
                                                          _
                                                          (Just (PartialConstraintData bs b)))) =
      paras [ line "A case expression could not be determined to cover all inputs."
            , line "The following additional cases are required to cover all inputs:"
            , indent $ paras $
                Box.hsep 1 Box.left
                  (map (paras . map (line . markCode)) (transpose bs))
                  : [line "..." | not b]
            , line "Alternatively, add a Partial constraint to the type of the enclosing value."
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint C.Discard [ty] _)) =
      paras [ line "A result of type"
            , markCodeBox $ indent $ typeAsBox ty
            , line "was implicitly discarded in a do notation block."
            , line ("You can use " <> markCode "_ <- ..." <> " to explicitly discard the result.")
            ]
    renderSimpleErrorMessage (NoInstanceFound (Constraint nm ts _)) =
      paras [ line "No type class instance was found for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
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
    renderSimpleErrorMessage (AmbiguousTypeVariables t _) =
      paras [ line "The inferred type"
            , markCodeBox $ indent $ typeAsBox t
            , line "has type variables which are not mentioned in the body of the type. Consider adding a type annotation."
            ]
    renderSimpleErrorMessage (PossiblyInfiniteInstance nm ts) =
      paras [ line "Type class instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line "is possibly infinite."
            ]
    renderSimpleErrorMessage (CannotDerive nm ts) =
      paras [ line "Cannot derive a type class instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line "since instances of this type class are not derivable."
            ]
    renderSimpleErrorMessage (InvalidNewtypeInstance nm ts) =
      paras [ line "Cannot derive newtype instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line "Make sure this is a newtype."
            ]
    renderSimpleErrorMessage (MissingNewtypeSuperclassInstance su cl ts) =
      paras [ line "The derived newtype instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName cl)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line $ "does not include a derived superclass instance for " <> markCode (showQualified runProperName su) <> "."
            ]
    renderSimpleErrorMessage (UnverifiableSuperclassInstance su cl ts) =
      paras [ line "The derived newtype instance for"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName cl)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line $ "implies an superclass instance for " <> markCode (showQualified runProperName su) <> " which could not be verified."
            ]
    renderSimpleErrorMessage (InvalidDerivedInstance nm ts argCount) =
      paras [ line "Cannot derive the type class instance"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , line $ fold $
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
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            , "because the type"
            , markCodeBox $ indent $ typeAsBox ty
            , line "is not of the required form T a_1 ... a_n, where T is a type constructor defined in the same module."
            ]
    renderSimpleErrorMessage (CannotFindDerivingType nm) =
      line $ "Cannot derive a type class instance, because the type declaration for " <> markCode (runProperName nm) <> " could not be found."
    renderSimpleErrorMessage (DuplicateLabel l expr) =
      paras $ [ line $ "Label " <> markCode (prettyPrintLabel l) <> " appears more than once in a row type." ]
                       <> foldMap (\expr' -> [ line "Relevant expression: "
                                             , markCodeBox $ indent $ prettyPrintValue valueDepth expr'
                                             ]) expr
    renderSimpleErrorMessage (DuplicateTypeArgument name) =
      line $ "Type argument " <> markCode name <> " appears more than once."
    renderSimpleErrorMessage (DuplicateValueDeclaration nm) =
      line $ "Multiple value declarations exist for " <> markCode (showIdent nm) <> "."
    renderSimpleErrorMessage (ArgListLengthsDiffer ident) =
      line $ "Argument list lengths differ in declaration " <> markCode (showIdent ident)
    renderSimpleErrorMessage (OverlappingArgNames ident) =
      line $ "Overlapping names in function/binder" <> foldMap ((" in declaration " <>) . showIdent) ident
    renderSimpleErrorMessage (MissingClassMember ident) =
      line $ "Type class member " <> markCode (showIdent ident) <> " has not been implemented."
    renderSimpleErrorMessage (ExtraneousClassMember ident className) =
      line $ "" <> markCode (showIdent ident) <> " is not a member of type class " <> markCode (showQualified runProperName className)
    renderSimpleErrorMessage (ExpectedType ty kind) =
      paras [ line $ "In a type-annotated expression " <> markCode "x :: t" <> ", the type " <> markCode "t" <> " must have kind " <> markCode (prettyPrintKind kindType) <> "."
            , line "The error arises from the type"
            , markCodeBox $ indent $ typeAsBox ty
            , line "having the kind"
            , indent $ line $ markCode $ prettyPrintKind kind
            , line "instead."
            ]
    renderSimpleErrorMessage (IncorrectConstructorArity nm) =
      line $ "Data constructor " <> markCode (showQualified runProperName nm) <> " was given the wrong number of arguments in a case expression."
    renderSimpleErrorMessage (ExprDoesNotHaveType expr ty) =
      paras [ line "Expression"
            , markCodeBox $ indent $ prettyPrintValue valueDepth expr
            , line "does not have type"
            , markCodeBox $ indent $ typeAsBox ty
            ]
    renderSimpleErrorMessage (PropertyIsMissing prop) =
      line $ "Type of expression lacks required label " <> markCode (prettyPrintLabel prop) <> "."
    renderSimpleErrorMessage (AdditionalProperty prop) =
      line $ "Type of expression contains additional label " <> markCode (prettyPrintLabel prop) <> "."
    renderSimpleErrorMessage TypeSynonymInstance =
      line "Type class instances for type synonyms are disallowed."
    renderSimpleErrorMessage (OrphanInstance nm cnm nonOrphanModules ts) =
      paras [ line $ "Orphan instance " <> markCode (showIdent nm) <> " found for "
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName cnm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
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
        formattedModules = T.intercalate " or " ((markCode . runModuleName) <$> modulesToList)
    renderSimpleErrorMessage (InvalidNewtype name) =
      paras [ line $ "Newtype " <> markCode (runProperName name) <> " is invalid."
            , line "Newtypes must define a single constructor with a single argument."
            ]
    renderSimpleErrorMessage (InvalidInstanceHead ty) =
      paras [ line "Type class instance head is invalid due to use of type"
            , markCodeBox $ indent $ typeAsBox ty
            , line "All types appearing in instance declarations must be of the form T a_1 .. a_n, where each type a_i is of the same form, unless the type is fully determined by other type class arguments via functional dependencies."
            ]
    renderSimpleErrorMessage (TransitiveExportError x ys) =
      paras [ line $ "An export for " <> markCode (prettyPrintExport x) <> " requires the following to also be exported: "
            , indent $ paras $ map (line . markCode . prettyPrintExport) ys
            ]
    renderSimpleErrorMessage (TransitiveDctorExportError x ctor) =
      paras [ line $ "An export for " <> markCode (prettyPrintExport x) <> " requires the following data constructor to also be exported: "
            , indent $ line $ markCode $ runProperName ctor
            ]
    renderSimpleErrorMessage (ShadowedName nm) =
      line $ "Name " <> markCode (showIdent nm) <> " was shadowed."
    renderSimpleErrorMessage (ShadowedTypeVar tv) =
      line $ "Type variable " <> markCode tv <> " was shadowed."
    renderSimpleErrorMessage (UnusedTypeVar tv) =
      line $ "Type variable " <> markCode tv <> " is ambiguous, since it is unused in the polymorphic type which introduces it."
    renderSimpleErrorMessage (MisleadingEmptyTypeImport mn name) =
      line $ "Importing type " <> markCode (runProperName name <> "(..)") <> " from " <> markCode (runModuleName mn) <> " is misleading as it has no exported data constructors."
    renderSimpleErrorMessage (ImportHidingModule name) =
      paras [ line "hiding imports cannot be used to hide modules."
            , line $ "An attempt was made to hide the import of " <> markCode (runModuleName name)
            ]
    renderSimpleErrorMessage (WildcardInferredType ty ctx) =
      paras $ [ line "Wildcard type definition has the inferred type "
              , markCodeBox $ indent $ typeAsBox ty
              ] <> renderContext ctx
    renderSimpleErrorMessage (HoleInferredType name ty ctx ts) =
      let
        maxTSResults = 15
        tsResult = case ts of
          (TSAfter{tsAfterIdentifiers=idents}) | not (null idents) ->
            let
              formatTS (names, types) =
                let
                  idBoxes = Box.text . T.unpack . showQualified id <$> names
                  tyBoxes = (\t -> BoxHelpers.indented
                              (Box.text ":: " Box.<> typeAsBox t)) <$> types
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
                , markCodeBox (indent (typeAsBox ty))
                ] ++ tsResult ++ renderContext ctx
    renderSimpleErrorMessage (MissingTypeDeclaration ident ty) =
      paras [ line $ "No type declaration was provided for the top-level declaration of " <> markCode (showIdent ident) <> "."
            , line "It is good practice to provide type declarations as a form of documentation."
            , line $ "The inferred type of " <> markCode (showIdent ident) <> " was:"
            , markCodeBox $ indent $ typeAsBox ty
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
      line $ "The import of module " <> markCode (runModuleName name) <> " is redundant"

    renderSimpleErrorMessage msg@(UnusedExplicitImport mn names _ _) =
      paras [ line $ "The import of module " <> markCode (runModuleName mn) <> " contains the following unused references:"
            , indent $ paras $ map (line . markCode . runName . Qualified Nothing) names
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
      line $ "Import list contains multiple references to " <> printName (Qualified Nothing name)

    renderSimpleErrorMessage (DuplicateExportRef name) =
      line $ "Export list contains multiple references to " <> printName (Qualified Nothing name)

    renderSimpleErrorMessage (IntOutOfRange value backend lo hi) =
      paras [ line $ "Integer value " <> markCode (T.pack (show value)) <> " is out of range for the " <> backend <> " backend."
            , line $ "Acceptable values fall within the range " <> markCode (T.pack (show lo)) <> " to " <> markCode (T.pack (show hi)) <> " (inclusive)." ]

    renderSimpleErrorMessage msg@(ImplicitQualifiedImport importedModule asModule _) =
      paras [ line $ "Module " <> markCode (runModuleName importedModule) <> " was imported as " <> markCode (runModuleName asModule) <> " with unspecified imports."
            , line $ "As there are multiple modules being imported as " <> markCode (runModuleName asModule) <> ", consider using the explicit form:"
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
            , markCodeBox $ indent $ typeAsBox ty
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
            , line $ "But the instance " <> markCode (showIdent dictName) <> mismatchMsg <> T.pack (show actual) <> "."
            ]
        where
          mismatchMsg = if actual > expected then " provided " else " only provided "
          argsMsg = if expected > 1 then "arguments" else "argument"

    renderSimpleErrorMessage (UserDefinedWarning msgTy) =
      let msg = fromMaybe (typeAsBox msgTy) (toTypelevelString msgTy) in
      paras [ line "A custom warning occurred while solving type class constraints:"
            , indent msg
            ]

    renderSimpleErrorMessage (UnusableDeclaration ident unexplained) =
      paras $
        [ line $ "The declaration " <> markCode (showIdent ident) <> " contains arguments that couldn't be determined."
        ] <>

        case unexplained of
          [required] ->
            [ line $ "These arguments are: { " <> T.intercalate "," required <> "}"
            ]

          options  ->
            [ line "To fix this, one of the following sets of variables must be determined:"
            , Box.moveRight 2 . Box.vsep 0 Box.top $
                map (\set -> line $ "{ " <> T.intercalate ", " set <> " }") options
            ]

    renderHint :: ErrorMessageHint -> Box.Box -> Box.Box
    renderHint (ErrorUnifyingTypes t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while trying to match type"
                                 , markCodeBox $ typeAsBox t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "with type"
                                                   , markCodeBox $ typeAsBox t2
                                                   ]
            ]
    renderHint (ErrorInExpression expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ Box.text "in the expression"
                                 , markCodeBox $ markCodeBox $ prettyPrintValue valueDepth expr
                                 ]
            ]
    renderHint (ErrorInModule mn) detail =
      paras [ line $ "in module " <> markCode (runModuleName mn)
            , detail
            ]
    renderHint (ErrorInSubsumption t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that type"
                                 , markCodeBox $ typeAsBox t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "is at least as general as type"
                                                   , markCodeBox $ typeAsBox t2
                                                   ]
            ]
    renderHint (ErrorInInstance nm ts) detail =
      paras [ detail
            , line "in type class instance"
            , markCodeBox $ indent $ Box.hsep 1 Box.top
               [ line $ showQualified runProperName nm
               , Box.vcat Box.left (map typeAtomAsBox ts)
               ]
            ]
    renderHint (ErrorCheckingKind ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking the kind of"
                                 , markCodeBox $ typeAsBox ty
                                 ]
            ]
    renderHint ErrorCheckingGuard detail =
      paras [ detail
            , line "while checking the type of a guard clause"
            ]
    renderHint (ErrorInferringType expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the type of"
                                 , markCodeBox $ prettyPrintValue valueDepth expr
                                 ]
            ]
    renderHint (ErrorCheckingType expr ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that expression"
                                 , markCodeBox $ prettyPrintValue valueDepth expr
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has type"
                                                   , markCodeBox $ typeAsBox ty
                                                   ]
            ]
    renderHint (ErrorCheckingAccessor expr prop) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking type of property accessor"
                                 , markCodeBox $ prettyPrintValue valueDepth (Accessor prop expr)
                                 ]
            ]
    renderHint (ErrorInApplication f t a) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while applying a function"
                                 , markCodeBox $ prettyPrintValue valueDepth f
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "of type"
                                                   , markCodeBox $ typeAsBox t
                                                   ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "to argument"
                                                   , markCodeBox $ prettyPrintValue valueDepth a
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
    renderHint (ErrorInForeignImport nm) detail =
      paras [ detail
            , line $ "in foreign import " <> markCode (showIdent nm)
            ]
    renderHint (ErrorSolvingConstraint (Constraint nm ts _)) detail =
      paras [ detail
            , line "while solving type class constraint"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map typeAtomAsBox ts)
                ]
            ]
    renderHint (PositionedError srcSpan) detail =
      paras [ line $ "at " <> displaySourceSpan relPath srcSpan
            , detail
            ]

    renderContext :: Context -> [Box.Box]
    renderContext [] = []
    renderContext ctx =
      [ line "in the following context:"
      , indent $ paras
          [ Box.hcat Box.left [ Box.text (T.unpack (showIdent ident) ++ " :: ")
                              , markCodeBox $ typeAsBox ty'
                              ]
          | (ident, ty') <- take 5 ctx
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
    nameType (KiName _) = "kind"

    runName :: Qualified Name -> Text
    runName (Qualified mn (IdentName name)) =
      showQualified showIdent (Qualified mn name)
    runName (Qualified mn (ValOpName op)) =
      showQualified showOp (Qualified mn op)
    runName (Qualified mn (TyName name)) =
      showQualified runProperName (Qualified mn name)
    runName (Qualified mn (TyOpName op)) =
      showQualified showOp (Qualified mn op)
    runName (Qualified mn (DctorName name)) =
      showQualified runProperName (Qualified mn name)
    runName (Qualified mn (TyClassName name)) =
      showQualified runProperName (Qualified mn name)
    runName (Qualified mn (KiName name)) =
      showQualified runProperName (Qualified mn name)
    runName (Qualified Nothing (ModName name)) =
      runModuleName name
    runName (Qualified _ ModName{}) =
      internalError "qualified ModName in runName"

  valueDepth :: Int
  valueDepth | full = 1000
             | otherwise = 3

  levelText :: Text
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
    stripRedudantHints ExprDoesNotHaveType{} = stripFirst isCheckHint
      where
      isCheckHint ErrorCheckingType{} = True
      isCheckHint _ = False
    stripRedudantHints TypesDoNotUnify{} = stripFirst isUnifyHint
      where
      isUnifyHint ErrorUnifyingTypes{} = True
      isUnifyHint _ = False
    stripRedudantHints NoInstanceFound{} = stripFirst isSolverHint
      where
      isSolverHint ErrorSolvingConstraint{} = True
      isSolverHint _ = False
    stripRedudantHints _ = id

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
  hintCategory _                                    = OtherHint

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
            Hiding refs -> runModuleName mn <> " hiding (" <> T.intercalate "," (mapMaybe prettyPrintRef refs) <> ")"
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
prettyPrintRef (TypeInstanceRef _ ident) =
  Just $ showIdent ident
prettyPrintRef (ModuleRef _ name) =
  Just $ "module " <> runModuleName name
prettyPrintRef (KindRef _ pn) =
  Just $ "kind " <> runProperName pn
prettyPrintRef ReExportRef{} =
  Nothing

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

-- | Pretty print a Parsec ParseError as a Box
prettyPrintParseError :: P.ParseError -> Box.Box
prettyPrintParseError = prettyPrintParseErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . PE.errorMessages

-- | Pretty print 'ParseError' detail messages.
--
-- Adapted from 'Text.Parsec.Error.showErrorMessages'.
-- See <https://github.com/aslatter/parsec/blob/v3.1.9/Text/Parsec/Error.hs#L173>.
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

  clean             = ordNub . filter (not . null)

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

toTypelevelString :: Type -> Maybe Box.Box
toTypelevelString (TypeLevelString s) =
  Just . Box.text $ decodeStringWithReplacement s
toTypelevelString (TypeApp (TypeConstructor f) x)
  | f == primName "TypeString" = Just (typeAsBox x)
toTypelevelString (TypeApp (TypeApp (TypeConstructor f) x) ret)
  | f == primName "TypeConcat" =
    (Box.<>) <$> toTypelevelString x <*> toTypelevelString ret
toTypelevelString _ = Nothing

-- | Rethrow an error with a more detailed error message in the case of failure
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError (throwError . f)

reifyErrors :: (MonadError e m) => m a -> m (Either e a)
reifyErrors ma = catchError (fmap Right ma) (return . Left)

reflectErrors :: (MonadError e m) => m (Either e a) -> m a
reflectErrors ma = ma >>= either throwError return

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
withPosition pos (ErrorMessage hints se) = ErrorMessage (PositionedError pos : hints) se

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
