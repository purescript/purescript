module Language.PureScript.Docs.Convert.Single
  ( convertSingleModule
  ) where

import Protolude hiding (moduleName)

import Control.Category ((>>>))

import qualified Data.Text as T

import Language.PureScript.Docs.Types
import qualified Language.PureScript as P

-- |
-- Convert a single Module, but ignore re-exports; any re-exported types or
-- values will not appear in the result.
--
convertSingleModule :: P.Module -> Module
convertSingleModule m@(P.Module _ coms moduleName  _ _) =
  Module moduleName comments (declarations m) []
  where
  comments = convertComments coms
  declarations =
    P.exportedDeclarations
    >>> mapMaybe (\d -> getDeclarationTitle d >>= convertDeclaration d)
    >>> augmentDeclarations

-- | Different declarations we can augment
data AugmentType
  = AugmentClass
  -- ^ Augment documentation for a type class
  | AugmentType
  -- ^ Augment documentation for a type constructor

-- | The data type for an intermediate stage which we go through during
-- converting.
--
-- In the first pass, we take all top level declarations in the module, and
-- collect other information which will later be used to augment the top level
-- declarations. These two situation correspond to the Right and Left
-- constructors, respectively.
--
-- In the second pass, we go over all of the Left values and augment the
-- relevant declarations, leaving only the augmented Right values.
--
-- Note that in the Left case, we provide a [Text] as well as augment
-- information. The [Text] value should be a list of titles of declarations
-- that the augmentation should apply to. For example, for a type instance
-- declaration, that would be any types or type classes mentioned in the
-- instance. For a fixity declaration, it would be just the relevant operator's
-- name.
type IntermediateDeclaration
  = Either ([(Text, AugmentType)], DeclarationAugment) Declaration

-- | Some data which will be used to augment a Declaration in the
-- output.
--
-- The AugmentChild constructor allows us to move all children under their
-- respective parents. It is only necessary for type instance declarations,
-- since they appear at the top level in the AST, and since they might need to
-- appear as children in two places (for example, if a data type defined in a
-- module is an instance of a type class also defined in that module).
data DeclarationAugment
  = AugmentChild ChildDeclaration

-- | Augment top-level declarations; the second pass. See the comments under
-- the type synonym IntermediateDeclaration for more information.
augmentDeclarations :: [IntermediateDeclaration] -> [Declaration]
augmentDeclarations (partitionEithers -> (augments, toplevels)) =
  foldl' go toplevels augments
  where
  go ds (parentTitles, a) =
    map (\d ->
      if any (matches d) parentTitles
        then augmentWith a d
        else d) ds

  matches d (name, AugmentType) = isType d && declTitle d == name
  matches d (name, AugmentClass) = isTypeClass d && declTitle d == name

  augmentWith (AugmentChild child) d =
    d { declChildren = declChildren d ++ [child] }

getDeclarationTitle :: P.Declaration -> Maybe Text
getDeclarationTitle (P.ValueDeclaration vd) = Just (P.showIdent (P.valdeclIdent vd))
getDeclarationTitle (P.ExternDeclaration _ name _) = Just (P.showIdent name)
getDeclarationTitle (P.DataDeclaration _ _ name _ _) = Just (P.runProperName name)
getDeclarationTitle (P.ExternDataDeclaration _ name _) = Just (P.runProperName name)
getDeclarationTitle (P.ExternKindDeclaration _ name) = Just (P.runProperName name)
getDeclarationTitle (P.TypeSynonymDeclaration _ name _ _) = Just (P.runProperName name)
getDeclarationTitle (P.TypeClassDeclaration _ name _ _ _ _) = Just (P.runProperName name)
getDeclarationTitle (P.TypeInstanceDeclaration _ _ _ name _ _ _ _) = Just (P.showIdent name)
getDeclarationTitle (P.TypeFixityDeclaration _ _ _ op) = Just ("type " <> P.showOp op)
getDeclarationTitle (P.ValueFixityDeclaration _ _ _ op) = Just (P.showOp op)
getDeclarationTitle _ = Nothing

-- | Create a basic Declaration value.
mkDeclaration :: P.SourceAnn -> Text -> DeclarationInfo -> Declaration
mkDeclaration (ss, com) title info =
  Declaration { declTitle      = title
              , declComments   = convertComments com
              , declSourceSpan = Just ss -- TODO: make this non-optional when we next break the format
              , declChildren   = []
              , declInfo       = info
              }

basicDeclaration :: P.SourceAnn -> Text -> DeclarationInfo -> Maybe IntermediateDeclaration
basicDeclaration sa title = Just . Right . mkDeclaration sa title

convertDeclaration :: P.Declaration -> Text -> Maybe IntermediateDeclaration
convertDeclaration (P.ValueDecl sa _ _ _ [P.MkUnguarded (P.TypedValue _ _ ty)]) title =
  basicDeclaration sa title (ValueDeclaration ty)
convertDeclaration (P.ValueDecl sa _ _ _ _) title =
  -- If no explicit type declaration was provided, insert a wildcard, so that
  -- the actual type will be added during type checking.
  basicDeclaration sa title (ValueDeclaration P.TypeWildcard{})
convertDeclaration (P.ExternDeclaration sa _ ty) title =
  basicDeclaration sa title (ValueDeclaration ty)
convertDeclaration (P.DataDeclaration sa dtype _ args ctors) title =
  Just (Right (mkDeclaration sa title info) { declChildren = children })
  where
  info = DataDeclaration dtype args
  children = map convertCtor ctors
  convertCtor (ctor', tys) =
    ChildDeclaration (P.runProperName ctor') Nothing Nothing (ChildDataConstructor tys)
convertDeclaration (P.ExternDataDeclaration sa _ kind') title =
  basicDeclaration sa title (ExternDataDeclaration kind')
convertDeclaration (P.ExternKindDeclaration sa _) title =
  basicDeclaration sa title ExternKindDeclaration
convertDeclaration (P.TypeSynonymDeclaration sa _ args ty) title =
  basicDeclaration sa title (TypeSynonymDeclaration args ty)
convertDeclaration (P.TypeClassDeclaration sa _ args implies fundeps ds) title =
  Just (Right (mkDeclaration sa title info) { declChildren = children })
  where
  info = TypeClassDeclaration args implies (convertFundepsToStrings args fundeps)
  children = map convertClassMember ds
  convertClassMember (P.TypeDeclaration (P.TypeDeclarationData (ss, com) ident' ty)) =
    ChildDeclaration (P.showIdent ident') (convertComments com) (Just ss) (ChildTypeClassMember ty)
  convertClassMember _ =
    P.internalError "convertDeclaration: Invalid argument to convertClassMember."
convertDeclaration (P.TypeInstanceDeclaration (ss, com) _ _ _ constraints className tys _) title =
  Just (Left ((classNameString, AugmentClass) : map (, AugmentType) typeNameStrings, AugmentChild childDecl))
  where
  classNameString = unQual className
  typeNameStrings = ordNub (concatMap (P.everythingOnTypes (++) extractProperNames) tys)
  unQual x = let (P.Qualified _ y) = x in P.runProperName y

  extractProperNames (P.TypeConstructor n) = [unQual n]
  extractProperNames _ = []

  childDecl = ChildDeclaration title (convertComments com) (Just ss) (ChildInstance constraints classApp)
  classApp = foldl' P.TypeApp (P.TypeConstructor (fmap P.coerceProperName className)) tys
convertDeclaration (P.ValueFixityDeclaration sa fixity (P.Qualified mn alias) _) title =
  Just . Right $ mkDeclaration sa title (AliasDeclaration fixity (P.Qualified mn (Right alias)))
convertDeclaration (P.TypeFixityDeclaration sa fixity (P.Qualified mn alias) _) title =
  Just . Right $ mkDeclaration sa title (AliasDeclaration fixity (P.Qualified mn (Left alias)))
convertDeclaration _ _ = Nothing

convertComments :: [P.Comment] -> Maybe Text
convertComments cs = do
  let raw = concatMap toLines cs
  let docs = mapMaybe stripPipe raw
  guard (not (null docs))
  pure (T.unlines docs)

  where
  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = T.lines s

  stripPipe =
    T.dropWhile (== ' ')
    >>> T.stripPrefix "|"
    >>> fmap (dropPrefix " ")

  dropPrefix prefix str =
    fromMaybe str (T.stripPrefix prefix str)
