{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap
  , replaceAllTypeSynonyms
  , replaceAllTypeSynonymsM
  ) where

import           Prelude.Compat

import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Data.Text (Text)
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Kinds
import           Language.PureScript.Names
import           Language.PureScript.TypeChecker.Monad
import           Language.PureScript.Types

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe (Kind SourceAnn))], Type SourceAnn)

replaceAllTypeSynonyms'
  :: SynonymMap
  -> Type SourceAnn
  -> Either MultipleErrors (Type SourceAnn)
replaceAllTypeSynonyms' syns = everywhereOnTypesTopDownM try
  where
  try :: Type SourceAnn -> Either MultipleErrors (Type SourceAnn)
  try t = fromMaybe t <$> go 0 [] t

  go :: Int -> [Type SourceAnn] -> Type SourceAnn -> Either MultipleErrors (Maybe (Type SourceAnn))
  go c args (TypeConstructor _ ctor)
    | Just (synArgs, body) <- M.lookup ctor syns
    , c == length synArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args) body
      in Just <$> try repl
    | Just (synArgs, _) <- M.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage $ PartiallyAppliedSynonym ctor
  go c args (TypeApp _ f arg) = go (c + 1) (arg : args) f
  go _ _ _ = return Nothing

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m) => Type SourceAnn -> m (Type SourceAnn)
replaceAllTypeSynonyms d = do
  env <- getEnv
  either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) d

-- | Replace fully applied type synonyms by explicitly providing a 'SynonymMap'.
replaceAllTypeSynonymsM
  :: MonadError MultipleErrors m
  => SynonymMap
  -> Type SourceAnn
  -> m (Type SourceAnn)
replaceAllTypeSynonymsM syns = either throwError pure . replaceAllTypeSynonyms' syns
