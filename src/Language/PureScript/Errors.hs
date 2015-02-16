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

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Language.PureScript.Errors where

import Data.Either (lefts, rights)
import Data.List (intersperse, intercalate)
import Data.Monoid

import Control.Monad.Error
import Control.Applicative ((<$>))

import Language.PureScript.AST
import Language.PureScript.Pretty
import Language.PureScript.Types

-- |
-- Type for sources of type checking errors
--
data ErrorSource
  -- |
  -- An error which originated at a Expr
  --
  = ExprError Expr
  -- |
  -- An error which originated at a Type
  --
  | TypeError Type deriving (Show)

-- |
-- Compilation errors
--
data CompileError
  = CompileError
      { -- |
        -- Error message
        --
        compileErrorMessage :: String
        -- |
        -- The value where the error occurred
        --
      , compileErrorValue :: Maybe ErrorSource
        -- |
        -- Optional source position information
        --
      , compileErrorPosition :: Maybe SourceSpan
      }
  deriving (Show)

-- |
-- A stack trace for an error
--
data ErrorStack
  = ErrorStack { runErrorStack :: [CompileError] }
  | MultipleErrors [ErrorStack] deriving (Show)

instance Monoid ErrorStack where
  mempty = ErrorStack []
  mappend (ErrorStack xs) (ErrorStack ys) = ErrorStack (xs ++ ys)
  mappend (MultipleErrors es) x = MultipleErrors [ e <> x | e <- es ]
  mappend x (MultipleErrors es) = MultipleErrors [ x <> e | e <- es ]

instance Error ErrorStack where
  strMsg s = ErrorStack [CompileError s Nothing Nothing]
  noMsg = ErrorStack []

prettyPrintErrorStack :: Bool -> ErrorStack -> String
prettyPrintErrorStack printFullStack (ErrorStack es) =
  case mconcat $ map (Last . compileErrorPosition) es of
    Last (Just sourcePos) -> "Error at " ++ show sourcePos ++ ": \n" ++ prettyPrintErrorStack'
    _ -> prettyPrintErrorStack'
  where
  prettyPrintErrorStack' :: String
  prettyPrintErrorStack'
    | printFullStack = intercalate "\n" (map showError (filter isErrorNonEmpty es))
    | otherwise =
      let
        es' = filter isErrorNonEmpty es
      in case length es' of
        1 -> showError (head es')
        _ -> showError (head es') ++ "\n" ++ showError (last es')
prettyPrintErrorStack printFullStack (MultipleErrors es) =
  unlines $ intersperse "" $ "Multiple errors:" : map (prettyPrintErrorStack printFullStack) es

stringifyErrorStack :: (MonadError String m) => Bool -> Either ErrorStack a -> m a
stringifyErrorStack printFullStack = either (throwError . prettyPrintErrorStack printFullStack) return

isErrorNonEmpty :: CompileError -> Bool
isErrorNonEmpty = not . null . compileErrorMessage

showError :: CompileError -> String
showError (CompileError msg Nothing _) = msg
showError (CompileError msg (Just (ExprError val)) _) = "Error in expression " ++ prettyPrintValue val ++ ":\n" ++ msg
showError (CompileError msg (Just (TypeError ty)) _) = "Error in type " ++ prettyPrintType ty ++ ":\n" ++ msg

mkErrorStack :: String -> Maybe ErrorSource -> ErrorStack
mkErrorStack msg t = ErrorStack [CompileError msg t Nothing]

positionError :: SourceSpan -> ErrorStack
positionError pos = ErrorStack [CompileError "" Nothing (Just pos)]

-- |
-- Rethrow an error with a more detailed error message in the case of failure
--
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

-- |
-- Rethrow an error with source position information
--
rethrowWithPosition :: (MonadError ErrorStack m) => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (positionError pos <>)

-- |
-- Collect errors in in parallel
--
parU :: (MonadError ErrorStack m, Functor m) => [a] -> (a -> m b) -> m [b]
parU xs f = forM xs (withError . f) >>= collectErrors
  where
  withError :: (MonadError ErrorStack m, Functor m) => m a -> m (Either ErrorStack a)
  withError u = catchError (Right <$> u) (return . Left)

  collectErrors :: (MonadError ErrorStack m, Functor m) => [Either ErrorStack a] -> m [a]
  collectErrors es = case lefts es of
    [err] -> throwError err
    [] -> return $ rights es
    errs -> throwError $ MultipleErrors errs
