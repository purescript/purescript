module Language.PureScript.Docs.Convert.Single
  ( convertSingleModule
  , collectBookmarks
  ) where

import Prelude.Compat

import Control.Category ((>>>))
import Control.Monad

import Data.Either
import Data.List (nub)
import Data.Maybe (mapMaybe)

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
-- Note that in the Left case, we provide a [String] as well as augment
-- information. The [String] value should be a list of titles of declarations
-- that the augmentation should apply to. For example, for a type instance
-- declaration, that would be any types or type classes mentioned in the
-- instance. For a fixity declaration, it would be just the relevant operator's
-- name.
type IntermediateDeclaration
  = Either ([String], DeclarationAugment) Declaration

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
  foldl go toplevels augments
  where
  go ds (parentTitles, a) =
    map (\d ->
      if declTitle d `elem` parentTitles
        then augmentWith a d
        else d) ds

  augmentWith (AugmentChild child) d =
    d { declChildren = declChildren d ++ [child] }

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.ValueDeclaration name _ _ _) = Just (P.showIdent name)
getDeclarationTitle (P.ExternDeclaration name _) = Just (P.showIdent name)
getDeclarationTitle (P.DataDeclaration _ name _ _) = Just (P.runProperName name)
getDeclarationTitle (P.ExternDataDeclaration name _) = Just (P.runProperName name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _) = Just (P.runProperName name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _) = Just (P.runProperName name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _) = Just (P.showIdent name)
getDeclarationTitle (P.TypeFixityDeclaration _ _ op) = Just ("type " ++ P.showOp op)
getDeclarationTitle (P.ValueFixityDeclaration _ _ op) = Just (P.showOp op)
getDeclarationTitle (P.PositionedDeclaration _ _ d) = getDeclarationTitle d
getDeclarationTitle _ = Nothing

-- | Create a basic Declaration value.
mkDeclaration :: String -> DeclarationInfo -> Declaration
mkDeclaration title info =
  Declaration { declTitle      = title
              , declComments   = Nothing
              , declSourceSpan = Nothing
              , declChildren   = []
              , declInfo       = info
              }

basicDeclaration :: String -> DeclarationInfo -> Maybe IntermediateDeclaration
basicDeclaration title info = Just $ Right $ mkDeclaration title info

convertDeclaration :: P.Declaration -> String -> Maybe IntermediateDeclaration
convertDeclaration (P.ValueDeclaration _ _ _ (Right (P.TypedValue _ _ ty))) title =
  basicDeclaration title (ValueDeclaration ty)
convertDeclaration P.ValueDeclaration{} title =
  -- If no explicit type declaration was provided, insert a wildcard, so that
  -- the actual type will be added during type checking.
  basicDeclaration title (ValueDeclaration P.TypeWildcard{})
convertDeclaration (P.ExternDeclaration _ ty) title =
  basicDeclaration title (ValueDeclaration ty)
convertDeclaration (P.DataDeclaration dtype _ args ctors) title =
  Just (Right (mkDeclaration title info) { declChildren = children })
  where
  info = DataDeclaration dtype args
  children = map convertCtor ctors
  convertCtor (ctor', tys) =
    ChildDeclaration (P.runProperName ctor') Nothing Nothing (ChildDataConstructor tys)
convertDeclaration (P.ExternDataDeclaration _ kind') title =
  basicDeclaration title (ExternDataDeclaration kind')
convertDeclaration (P.TypeSynonymDeclaration _ args ty) title =
  basicDeclaration title (TypeSynonymDeclaration args ty)
convertDeclaration (P.TypeClassDeclaration _ args implies ds) title =
  Just (Right (mkDeclaration title info) { declChildren = children })
  where
  info = TypeClassDeclaration args implies
  children = map convertClassMember ds
  convertClassMember (P.PositionedDeclaration _ _ d) =
    convertClassMember d
  convertClassMember (P.TypeDeclaration ident' ty) =
    ChildDeclaration (P.showIdent ident') Nothing Nothing (ChildTypeClassMember ty)
  convertClassMember _ =
    P.internalError "convertDeclaration: Invalid argument to convertClassMember."
convertDeclaration (P.TypeInstanceDeclaration _ constraints className tys _) title =
  Just (Left (classNameString : typeNameStrings, AugmentChild childDecl))
  where
  classNameString = unQual className
  typeNameStrings = nub (concatMap (P.everythingOnTypes (++) extractProperNames) tys)
  unQual x = let (P.Qualified _ y) = x in P.runProperName y

  extractProperNames (P.TypeConstructor n) = [unQual n]
  extractProperNames _ = []

  childDecl = ChildDeclaration title Nothing Nothing (ChildInstance constraints classApp)
  classApp = foldl P.TypeApp (P.TypeConstructor (fmap P.coerceProperName className)) tys
convertDeclaration (P.ValueFixityDeclaration fixity (P.Qualified mn alias) _) title =
  Just $ Right $ mkDeclaration title (AliasDeclaration fixity (P.Qualified mn (Right alias)))
convertDeclaration (P.TypeFixityDeclaration fixity (P.Qualified mn alias) _) title =
  Just $ Right $ mkDeclaration title (AliasDeclaration fixity (P.Qualified mn (Left alias)))
convertDeclaration (P.PositionedDeclaration srcSpan com d') title =
  fmap (addComments . addSourceSpan) (convertDeclaration d' title)
  where
  addComments (Right d) =
    Right (d { declComments = convertComments com })
  addComments (Left augment) =
    Left (withAugmentChild (\d -> d { cdeclComments = convertComments com })
                           augment)

  addSourceSpan (Right d) =
    Right (d { declSourceSpan = Just srcSpan })
  addSourceSpan (Left augment) =
    Left (withAugmentChild (\d -> d { cdeclSourceSpan = Just srcSpan })
                           augment)

  withAugmentChild f (t, AugmentChild d) = (t, AugmentChild (f d))
convertDeclaration _ _ = Nothing

convertComments :: [P.Comment] -> Maybe String
convertComments cs = do
  let raw = concatMap toLines cs
  let docs = mapMaybe stripPipe raw
  guard (not (null docs))
  pure (unlines docs)

  where
  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

  stripPipe s' =
    case dropWhile (== ' ') s' of
      ('|':' ':s) ->
        Just s
      ('|':s) ->
        Just s
      _ ->
        Nothing

-- | Go through a PureScript module and extract a list of Bookmarks; references
-- to data types or values, to be used as a kind of index. These are used for
-- generating links in the HTML documentation, for example.
collectBookmarks :: InPackage P.Module -> [Bookmark]
collectBookmarks (Local m) = map Local (collectBookmarks' m)
collectBookmarks (FromDep pkg m) = map (FromDep pkg) (collectBookmarks' m)

collectBookmarks' :: P.Module -> [(P.ModuleName, String)]
collectBookmarks' m =
  map (P.getModuleName m, )
      (mapMaybe getDeclarationTitle
                (P.exportedDeclarations m))
