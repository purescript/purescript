{-# LANGUAGE DataKinds #-}

module Language.PureScript.Interactive.Completion
  ( CompletionM
  , liftCompletionM
  , completion
  , completion'
  ) where

import Prelude.Compat

import           Control.Monad ((<=<))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Reader (asks, runReaderT, ReaderT)
import           Data.List (nub, isPrefixOf, sortBy, stripPrefix)
import           Data.Maybe (mapMaybe)
import qualified Language.PureScript as P
import qualified Language.PureScript.Interactive.Directive as D
import           Language.PureScript.Interactive.Types
import qualified Language.PureScript.Names as N
import           System.Console.Haskeline
import Debug.Trace

-- Completions may read the state, but not modify it.
type CompletionM = ReaderT PSCiState IO

-- Lift a `CompletionM` action into a state monad.
liftCompletionM
  :: (MonadState PSCiState m, MonadIO m)
  => CompletionM a
  -> m a
liftCompletionM act = do
  st <- get
  liftIO $ runReaderT act st

-- Haskeline completions

-- | Loads module, function, and file completions.
completion
  :: (MonadState PSCiState m, MonadIO m)
  => CompletionFunc m
completion = liftCompletionM . completion'

completion' :: CompletionFunc CompletionM
completion' = completeWordWithPrev Nothing " \t\n\r" findCompletions

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

    getCompletion :: CompletionContext -> CompletionM [Either String Completion]
    getCompletion ctx =
      case ctx of
        CtxFilePath f        -> map Right <$> listFiles f
        CtxModule            -> map Left <$> getModuleNames
        CtxIdentifier        -> map Left <$> ((++) <$> getIdentNames <*> getDctorNames)
        CtxType              -> map Left <$> getTypeNames
        CtxFixed str         -> return [Left str]
        CtxDirective d       -> return (map Left (completeDirectives d))

    completeDirectives :: String -> [String]
    completeDirectives = map (':' :) . D.directiveStringsFor

    prefixedBy :: String -> String -> Maybe Completion
    prefixedBy w cand = if w `isPrefixOf` cand
                          then Just (simpleCompletion cand)
                          else Nothing

    directivesFirst :: Completion -> Completion -> Ordering
    directivesFirst (Completion _ d1 _) (Completion _ d2 _) = go d1 d2
      where
      go (':' : xs) (':' : ys) = compare xs ys
      go (':' : _) _ = LT
      go _ (':' : _) = GT
      go xs ys = compare xs ys

data CompletionContext
  = CtxDirective String
  | CtxFilePath String
  | CtxModule
  | CtxIdentifier
  | CtxType
  | CtxFixed String
  deriving (Show, Read)

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
directiveArg _ Quit        = []
directiveArg _ Reset       = []
directiveArg _ Help        = []
directiveArg _ Show        = map CtxFixed replQueryStrings
directiveArg _ Type        = [CtxIdentifier]
directiveArg _ Kind        = [CtxType]

completeImport :: [String] -> String -> [CompletionContext]
completeImport ws w' =
  case (ws, w') of
    (["import"], _) -> [CtxModule]
    _               -> []

headSatisfies :: (a -> Bool) -> [a] -> Bool
headSatisfies p str =
  case str of
    (c:_)  -> p c
    _     -> False

getLoadedModules :: CompletionM [P.Module]
getLoadedModules = asks (map fst . psciLoadedExterns)

getModuleNames :: CompletionM [String]
getModuleNames = moduleNames <$> getLoadedModules

mapLoadedModulesAndQualify :: (a -> String) -> ([P.DeclarationRef] -> [a]) -> CompletionM [String]
mapLoadedModulesAndQualify sho f = do
  ms <- getLoadedModules
  let
    argPairs = do
      m <- ms
      case m of
        P.Module _ _ _ _ Nothing -> do
          traceShow ("error", P.getModuleName m) $ return ()
          P.internalError "unelaborated module exports in mapLoadedModulesAndQualify"
        P.Module _ _ _ _ (Just refs) -> do
          traceShow ("non-error", P.getModuleName m) $ return ()
          fm <- f refs
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
getAllQualifications :: (a -> String) -> P.Module -> a -> CompletionM [String]
getAllQualifications sho m declName = do
  imports <- getAllImportsOf m
  let fullyQualified = qualifyWith (Just (P.getModuleName m))
  let otherQuals = nub (concatMap qualificationsUsing imports)
  return $ fullyQualified : otherQuals
  where
  qualifyWith mMod = P.showQualified sho (P.Qualified mMod declName)
  qualificationsUsing (_, _, asQ') = [qualifyWith asQ']


-- | Returns all the ImportedModule values referring to imports of a particular
-- module.
getAllImportsOf :: P.Module -> CompletionM [ImportedModule]
getAllImportsOf = asks . allImportsOf

typeDecls :: [P.DeclarationRef] -> [N.ProperName 'N.TypeName]
typeDecls = mapMaybe (fmap fst . P.getTypeRef)

identNames :: [P.DeclarationRef] -> [N.Ident]
identNames = mapMaybe P.getValueRef

dctorNames :: [P.DeclarationRef] -> [N.ProperName 'N.ConstructorName]
dctorNames = concat . mapMaybe (snd <=< P.getTypeRef)

moduleNames :: [P.Module] -> [String]
moduleNames = nub . map (P.runModuleName . P.getModuleName)
