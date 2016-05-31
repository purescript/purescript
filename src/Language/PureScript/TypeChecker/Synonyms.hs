{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.TypeChecker.Synonyms
  ( replaceAllTypeSynonyms
  ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- |
-- Replace fully applied type synonyms.
--
replaceAllTypeSynonyms' :: Environment -> Type -> Either MultipleErrors Type
replaceAllTypeSynonyms' env = everywhereOnTypesTopDownM try
  where
  try :: Type -> Either MultipleErrors Type
  try t = fromMaybe t <$> go 0 [] t

  go :: Int -> [Type] -> Type -> Either MultipleErrors (Maybe Type)
  go c args (TypeConstructor ctor)
    | Just (synArgs, body) <- M.lookup ctor (typeSynonyms env)
    , c == length synArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args) body
      in Just <$> try repl
    | Just (synArgs, _) <- M.lookup ctor (typeSynonyms env)
    , length synArgs > c
    = throwError . errorMessage $ PartiallyAppliedSynonym ctor
  go c args (TypeApp f arg) = go (c + 1) (arg : args) f
  go _ _ _ = return Nothing

replaceAllTypeSynonyms :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m) => Type -> m Type
replaceAllTypeSynonyms d = do
  env <- getEnv
  either throwError return $ replaceAllTypeSynonyms' env d
