module Language.PureScript.Interactive.Completion
  ( CompletionM
  , liftCompletionM
  , completion
  , completion'
  , formatCompletions
  ) where

import Prelude.Compat
import Protolude (ordNub)

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Reader (asks, runReaderT, ReaderT)
import           Data.List (nub, isPrefixOf, isInfixOf, isSuffixOf, sortBy, stripPrefix)
import           Data.Map (keys)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Interactive.Directive as D
import           Language.PureScript.Interactive.Types
import           System.Console.Haskeline

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
completion' = completeWordWithPrev Nothing " \t\n\r([" findCompletions

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
        CtxType pre          -> map (Left . (pre ++)) <$> getTypeNames
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

-- |
-- Convert Haskeline completion result to results as they would be displayed
formatCompletions :: (String, [Completion]) -> [String]
formatCompletions (unusedR, completions) = actuals
  where
    unused = reverse unusedR
    actuals = map ((unused ++) . replacement) completions

data CompletionContext
  = CtxDirective String
  | CtxFilePath String
  | CtxModule
  | CtxIdentifier
  | CtxType String
  | CtxFixed String
  deriving (Show)

-- |
-- Decide what kind of completion we need based on input. This function expects
-- a list of complete words (to the left of the cursor) as the first argument,
-- and the current word as the second argument.
completionContext :: [String] -> String -> [CompletionContext]
completionContext _ w  | "::" `isInfixOf` w = [CtxType (w `endingWith` "::")]
completionContext ws _ | lastSatisfies ("::" `isSuffixOf`) ws = [CtxType ""]
completionContext [] _ = [CtxDirective "", CtxIdentifier, CtxFixed "import"]
completionContext ws w | headSatisfies (":" `isPrefixOf`) ws = completeDirective ws w
completionContext ws w | headSatisfies (== "import") ws = completeImport ws w
completionContext _ _ = [CtxIdentifier]

endingWith :: String -> String -> String
endingWith str stop = aux "" str
  where
  aux acc s@(x:xs)
    | stop `isPrefixOf` s = reverse (stop ++ acc)
    | otherwise           = aux (x:acc) xs
  aux acc []              = reverse (stop ++ acc)

completeDirective :: [String] -> String -> [CompletionContext]
completeDirective ws w =
  case ws of
    []     -> [CtxDirective w]
    (x:xs) -> case D.directivesFor <$> stripPrefix ":" x of
                 -- only offer completions if the directive is unambiguous
                 Just [dir] -> directiveArg xs dir
                 _          -> []

directiveArg :: [String] -> Directive -> [CompletionContext]
directiveArg [] Browse = [CtxModule]                    -- only complete very next term
directiveArg [] Show   = map CtxFixed replQueryStrings  -- only complete very next term
directiveArg _ Type    = [CtxIdentifier]
directiveArg _ Kind    = [CtxType ""]
directiveArg _ _       = []

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

lastSatisfies :: (a -> Bool) -> [a] -> Bool
lastSatisfies _ [] = False
lastSatisfies p xs = p (last xs)

getLoadedModules :: CompletionM [P.Module]
getLoadedModules = asks (map fst . psciLoadedExterns)

getModuleNames :: CompletionM [String]
getModuleNames = moduleNames <$> getLoadedModules

getIdentNames :: CompletionM [String]
getIdentNames = do
  importedVals <- asks (keys . P.importedValues . psciImports)
  exportedVals <- asks (keys . P.exportedValues . psciExports)

  importedValOps <- asks (keys . P.importedValueOps . psciImports)
  exportedValOps <- asks (keys . P.exportedValueOps . psciExports)

  return . nub $ map (T.unpack . P.showQualified P.showIdent) importedVals
              ++ map (T.unpack . P.showQualified P.runOpName) importedValOps
              ++ map (T.unpack . P.showIdent) exportedVals
              ++ map (T.unpack . P.runOpName) exportedValOps

getDctorNames :: CompletionM [String]
getDctorNames = do
  imports <- asks (keys . P.importedDataConstructors . psciImports)
  return . nub $ map (T.unpack . P.showQualified P.runProperName) imports

getTypeNames :: CompletionM [String]
getTypeNames = do
  importedTypes <- asks (keys . P.importedTypes . psciImports)
  exportedTypes <- asks (keys . P.exportedTypes . psciExports)

  importedTypeOps <- asks (keys . P.importedTypeOps . psciImports)
  exportedTypeOps <- asks (keys . P.exportedTypeOps . psciExports)

  return . nub $ map (T.unpack . P.showQualified P.runProperName) importedTypes
              ++ map (T.unpack . P.showQualified P.runOpName) importedTypeOps
              ++ map (T.unpack . P.runProperName) exportedTypes
              ++ map (T.unpack . P.runOpName) exportedTypeOps

moduleNames :: [P.Module] -> [String]
moduleNames = ordNub . map (T.unpack . P.runModuleName . P.getModuleName)
