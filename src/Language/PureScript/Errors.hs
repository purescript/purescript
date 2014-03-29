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
import Language.PureScript.Names
import Language.PureScript.Pretty
import Language.PureScript.Types

-- |
-- Type for sources of type checking errors
--
data ErrorSource
  -- |
  -- An error which originated in a module
  --
  = ModuleError ModuleName
  -- |
  -- An error which originated at a Declaration
  --
  | DeclarationError (Either ProperName Ident)
  -- |
  -- An error which originated at a Value
  --
  | ValueError Value
  -- |
  -- An error which originated at a Type
  --
  | TypeError Type deriving (Show)

-- |
-- Compilation errors
--
data CompileError = CompileError {
    -- |
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
  , compileErrorPosition :: Maybe SourcePos
  } deriving (Show)

-- |
-- A stack trace for an error
--
newtype ErrorStack = ErrorStack { runUnifyErrorStack :: [CompileError] } deriving (Show, Monoid)

instance Error ErrorStack where
  strMsg s = ErrorStack [CompileError s Nothing Nothing]
  noMsg = ErrorStack []

prettyPrintErrorStack :: Bool -> ErrorStack -> String
prettyPrintErrorStack printAll (ErrorStack es) =
  case mconcat $ map (Last . compileErrorPosition) es of
    Last (Just sourcePos) -> "Error at " ++ show sourcePos ++ ": \n" ++ prettyPrintUnifyErrorStack'
    _ -> prettyPrintUnifyErrorStack'
  where
  prettyPrintUnifyErrorStack' :: String
  prettyPrintUnifyErrorStack'
    | printAll = intercalate "\n" (map showError (filter isErrorNonEmpty es))
    | otherwise =
      let
        es' = filter isErrorNonEmpty es
      in case length es' of
        1 -> showError (head es')
        _ -> showError (head es') ++ "\n" ++ showError (last es')

isErrorNonEmpty :: CompileError -> Bool
isErrorNonEmpty = not . null . compileErrorMessage

showError :: CompileError -> String
showError (CompileError msg Nothing _) = msg
showError (CompileError msg (Just (ModuleError mn)) _) = "Error in module " ++ runModuleName mn ++ ":\n" ++ msg
showError (CompileError msg (Just (DeclarationError (Left pn))) _) = "Error in declaration " ++ show pn ++ ":\n" ++ msg
showError (CompileError msg (Just (DeclarationError (Right i))) _) = "Error in declaration " ++ show i ++ ":\n" ++ msg
showError (CompileError msg (Just (ValueError val)) _) = "Error in value " ++ prettyPrintValue val ++ ":\n" ++ msg
showError (CompileError msg (Just (TypeError ty)) _) = "Error in type " ++ prettyPrintType ty ++ ":\n" ++ msg

mkUnifyErrorStack :: String -> Maybe ErrorSource -> ErrorStack
mkUnifyErrorStack msg t = ErrorStack [CompileError msg t Nothing]

positionError :: SourcePos -> ErrorStack
positionError pos = ErrorStack [CompileError "" Nothing (Just pos)]
