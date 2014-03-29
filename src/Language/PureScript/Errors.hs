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

module Language.PureScript.Errors where

import Data.List (intercalate)
import Data.Monoid

import Control.Monad.Error

import Language.PureScript.Declarations
import Language.PureScript.Options
import Language.PureScript.Pretty
import Language.PureScript.Types

-- |
-- Type for sources of type checking errors
--
data UnifyErrorSource
  -- |
  -- An error which originated at a Value
  --
  = ValueError Value
  -- |
  -- An error which originated at a Type
  --
  | TypeError Type deriving (Show)

-- |
-- Unification errors
--
data UnifyError = UnifyError {
    -- |
    -- Error message
    --
    unifyErrorMessage :: String
    -- |
    -- The value where the error occurred
    --
  , unifyErrorValue :: Maybe UnifyErrorSource
    -- |
    -- Optional source position information
    --
  , unifyErrorPosition :: Maybe SourcePos
  } deriving (Show)

-- |
-- A stack trace for an error
--
newtype UnifyErrorStack = UnifyErrorStack { runUnifyErrorStack :: [UnifyError] } deriving (Show, Monoid)

instance Error UnifyErrorStack where
  strMsg s = UnifyErrorStack [UnifyError s Nothing Nothing]
  noMsg = UnifyErrorStack []

prettyPrintUnifyErrorStack :: Options -> UnifyErrorStack -> String
prettyPrintUnifyErrorStack opts (UnifyErrorStack es) =
  case mconcat $ map (Last . unifyErrorPosition) es of
    Last (Just sourcePos) -> "Error at " ++ show sourcePos ++ ": \n" ++ prettyPrintUnifyErrorStack'
    _ -> prettyPrintUnifyErrorStack'
  where
  prettyPrintUnifyErrorStack' :: String
  prettyPrintUnifyErrorStack'
    | optionsVerboseErrors opts =
      intercalate "\n" (map showError (filter isErrorNonEmpty es))
    | otherwise =
      let
        es' = filter isErrorNonEmpty es
      in case length es' of
        1 -> showError (head es')
        _ -> showError (head es') ++ "\n" ++ showError (last es')

isErrorNonEmpty :: UnifyError -> Bool
isErrorNonEmpty = not . null . unifyErrorMessage

showError :: UnifyError -> String
showError (UnifyError msg Nothing _) = msg
showError (UnifyError msg (Just (ValueError val)) _) = "Error in value " ++ prettyPrintValue val ++ "\n" ++ msg
showError (UnifyError msg (Just (TypeError ty)) _) = "Error in type " ++ prettyPrintType ty ++ "\n" ++ msg

mkUnifyErrorStack :: String -> Maybe UnifyErrorSource -> UnifyErrorStack
mkUnifyErrorStack msg t = UnifyErrorStack [UnifyError msg t Nothing]

positionError :: SourcePos -> UnifyErrorStack
positionError pos = UnifyErrorStack [UnifyError "" Nothing (Just pos)]
