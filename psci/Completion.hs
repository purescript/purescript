{-# LANGUAGE CPP #-}

module Completion where

import Data.Maybe (mapMaybe)
import Data.List (nub, nubBy, sortBy, isPrefixOf, stripPrefix)
import Data.Char (isUpper)
import Data.Function (on)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif

import Control.Arrow (second)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad.Trans.Reader (asks, runReaderT, ReaderT)
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline

import qualified Language.PureScript as P
import qualified Language.PureScript.Names as N

import qualified Directive as D
import Types

-- Completions may read the state, but not modify it.
type CompletionM = ReaderT PSCiState IO

-- Lift a `CompletionM` action to a `StateT PSCiState IO` one.
liftCompletionM :: CompletionM a -> StateT PSCiState IO a
liftCompletionM act = StateT (\s -> (\a -> (a, s)) <$> runReaderT act s)

-- Haskeline completions

data CompletionContext
  = CtxDirective String
  | CtxFilePath String
  | CtxModule
  | CtxIdentifier
  | CtxType
  | CtxFixed String
  deriving (Show, Read)

-- |
-- Loads module, function, and file completions.
--
completion :: CompletionFunc (StateT PSCiState IO)
completion = liftCompletionM . completion'

completion' :: CompletionFunc CompletionM
completion' = completeWordWithPrev Nothing " \t\n\r" findCompletions

-- |
-- Decide what kind of completion we need based on input. This function expects
-- a list of complete words (to the left of the cursor) as the first argument,
-- and the current word as the second argument.
completionContext :: [String] -> String -> [CompletionContext]
completionContext [] _ = [CtxDirective "", CtxIdentifier, CtxFixed "import"]
completionContext ws w | headSatisfies (":" `isPrefixOf`) ws = completeDirective ws w
completionContext ws w | headSatisfies (== "import") ws = completeImport ws w
completionContext _ _ = [CtxIdentifier]

completeDirective :: [String] -> String -> [CompletionContext]
completeDirective ws w =
  case ws of
    []    -> [CtxDirective w]
    [dir] -> case D.directivesFor <$> stripPrefix ":" dir of
                -- only offer completions if the directive is unambiguous
                Just [dir'] -> directiveArg w dir'
                _           -> []

    -- All directives take exactly one argument. If we haven't yet matched,
    -- that means one argument has already been supplied. So don't complete
    -- any others.
    _     -> []

directiveArg :: String -> Directive -> [CompletionContext]
directiveArg _ Browse      = [CtxModule]
directiveArg w Load        = [CtxFilePath w]
directiveArg w Foreign     = [CtxFilePath w]
directiveArg _ Quit        = []
directiveArg _ Reset       = []
directiveArg _ Help        = []
directiveArg _ Show        = map CtxFixed replQueryStrings
directiveArg _ Type        = [CtxIdentifier]
directiveArg _ Kind        = [CtxType]

completeImport :: [String] -> String -> [CompletionContext]
completeImport ws w' =
  case (ws, w') of
    (["import"], w) | headSatisfies isUpper w -> [CtxModule]
    (["import"], _)                           -> [CtxModule, CtxFixed "qualified"]
    (["import", "qualified"], _)              -> [CtxModule]
    _                                         -> []

headSatisfies :: (a -> Bool) -> [a] -> Bool
headSatisfies p str =
  case str of
    (c:_)  -> p c
    _     -> False

-- | Callback for Haskeline's `completeWordWithPrev`.
-- Expects:
--   * Line contents to the left of the word, reversed
--   * Word to be completed
findCompletions :: String -> String -> CompletionM [Completion]
findCompletions prev word = do
  let ctx = completionContext (words (reverse prev)) word
  completions <- concat <$> traverse getCompletions ctx
  return $ sortBy directivesFirst completions
  where
  getCompletions :: CompletionContext -> CompletionM [Completion]
  getCompletions = fmap (mapMaybe (either (prefixedBy word) Just)) . getCompletion

  prefixedBy :: String -> String -> Maybe Completion
  prefixedBy w cand = if w `isPrefixOf` cand
                        then Just (simpleCompletion cand)
                        else Nothing

getCompletion :: CompletionContext -> CompletionM [Either String Completion]
getCompletion ctx =
  case ctx of
    CtxFilePath f        -> map Right <$> listFiles f
    CtxModule            -> map Left <$> getModuleNames
    CtxIdentifier        -> map Left <$> ((++) <$> getIdentNames <*> getDctorNames)
    CtxType              -> map Left <$> getTypeNames
    CtxFixed str         -> return [Left str]
    CtxDirective d       -> return (map Left (completeDirectives d))

  where
  completeDirectives :: String -> [String]
  completeDirectives = map (':' :) . D.directiveStringsFor


getLoadedModules :: CompletionM [P.Module]
getLoadedModules = asks (map snd . psciLoadedModules)

getImportedModules :: CompletionM [ImportedModule]
getImportedModules = asks psciImportedModules

getModuleNames :: CompletionM [String]
getModuleNames = moduleNames <$> getLoadedModules

mapLoadedModulesAndQualify :: (a -> String) -> (P.Module -> [(a, P.Declaration)]) -> CompletionM [String]
mapLoadedModulesAndQualify sho f = do
  ms <- getLoadedModules
  let argPairs = do m <- ms
                    fm <- f m
                    return (m, fm)
  concat <$> traverse (uncurry (getAllQualifications sho)) argPairs

getIdentNames :: CompletionM [String]
getIdentNames = mapLoadedModulesAndQualify P.showIdent identNames

getDctorNames :: CompletionM [String]
getDctorNames = mapLoadedModulesAndQualify P.runProperName dctorNames

getTypeNames :: CompletionM [String]
getTypeNames = mapLoadedModulesAndQualify P.runProperName typeDecls

-- | Given a module and a declaration in that module, return all possible ways
-- it could have been referenced given the current PSCiState - including fully
-- qualified, qualified using an alias, and unqualified.
getAllQualifications :: (a -> String) -> P.Module -> (a, P.Declaration) -> CompletionM [String]
getAllQualifications sho m (declName, decl) = do
  imports <- getAllImportsOf m
  let fullyQualified = qualifyWith (Just (P.getModuleName m))
  let otherQuals = nub (concatMap qualificationsUsing imports)
  return $ fullyQualified : otherQuals
  where
  qualifyWith mMod = P.showQualified sho (P.Qualified mMod declName)
  referencedBy refs = P.isExported (Just refs) decl

  qualificationsUsing (_, importType, asQ') =
    let q = qualifyWith asQ'
    in case importType of
          P.Implicit      -> [q]
          P.Explicit refs -> if referencedBy refs
                               then [q]
                               else []
          P.Hiding refs   -> if referencedBy refs
                               then []
                               else [q]


-- | Returns all the ImportedModule values referring to imports of a particular
-- module.
getAllImportsOf :: P.Module -> CompletionM [ImportedModule]
getAllImportsOf = asks . allImportsOf

nubOnFst :: Eq a => [(a, b)] -> [(a, b)]
nubOnFst = nubBy ((==) `on` fst)

typeDecls :: P.Module -> [(N.ProperName, P.Declaration)]
typeDecls = mapMaybe getTypeName . filter P.isDataDecl . P.exportedDeclarations
  where
  getTypeName :: P.Declaration -> Maybe (N.ProperName, P.Declaration)
  getTypeName d@(P.TypeSynonymDeclaration name _ _) = Just (name, d)
  getTypeName d@(P.DataDeclaration _ name _ _) = Just (name, d)
  getTypeName (P.PositionedDeclaration _ _ d) = getTypeName d
  getTypeName _ = Nothing

identNames :: P.Module -> [(N.Ident, P.Declaration)]
identNames = nubOnFst . concatMap getDeclNames . P.exportedDeclarations
  where
  getDeclNames :: P.Declaration -> [(P.Ident, P.Declaration)]
  getDeclNames d@(P.ValueDeclaration ident _ _ _)  = [(ident, d)]
  getDeclNames d@(P.TypeDeclaration ident _ ) = [(ident, d)]
  getDeclNames d@(P.ExternDeclaration ident _) = [(ident, d)]
  getDeclNames d@(P.TypeClassDeclaration _ _ _ ds) = map (second (const d)) $ concatMap getDeclNames ds
  getDeclNames (P.PositionedDeclaration _ _ d) = getDeclNames d
  getDeclNames _ = []

dctorNames :: P.Module -> [(N.ProperName, P.Declaration)]
dctorNames = nubOnFst . concatMap go . P.exportedDeclarations
  where
  go :: P.Declaration -> [(N.ProperName, P.Declaration)]
  go decl@(P.DataDeclaration _ _ _ ctors) = map (\n -> (n, decl)) (map fst ctors)
  go (P.PositionedDeclaration _ _ d) = go d
  go _ = []

moduleNames :: [P.Module] -> [String]
moduleNames ms = nub [P.runModuleName moduleName | P.Module _ _ moduleName _ _ <- ms]

directivesFirst :: Completion -> Completion -> Ordering
directivesFirst (Completion _ d1 _) (Completion _ d2 _) = go d1 d2
  where
  go (':' : xs) (':' : ys) = compare xs ys
  go (':' : _) _ = LT
  go _ (':' : _) = GT
  go xs ys = compare xs ys
