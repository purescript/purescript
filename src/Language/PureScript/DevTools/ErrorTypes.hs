{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.DevTools.ErrorTypes where

import Control.Monad.Error
import Language.PureScript.Declarations
import Language.PureScript.Types
import Data.Monoid
import Data.Aeson hiding (Value)

-- |
-- Type for sources of type checking errors
--
data ErrorSource
  -- |
  -- An error which originated at a Value
  --
  = ValueError Value
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
newtype ErrorStack = ErrorStack { runErrorStack :: [CompileError] } deriving (Show, Monoid)

instance Error ErrorStack where
  strMsg s = ErrorStack [CompileError s Nothing Nothing]
  noMsg = ErrorStack []

-- Providing JSON converters for purs stacks

instance ToJSON ErrorSource where
  toJSON (ValueError v) = object ["type" .= show v, "info" .= show v]
  toJSON (TypeError t) = object ["type" .=  show t, "info" .= show t]

instance ToJSON CompileError where
  toJSON (CompileError msg v (Just (SourcePos n l c))) = object [
    "errorMessage" .= msg,
    "value" .= v,
    "pos" .= object [ "name" .= n, "line" .= l, "col" .= c]]
  toJSON (CompileError msg v _) = object [
    "errorMessage" .= msg,
    "value" .= v,
    "pos" .= Null]

instance ToJSON ErrorStack where
  toJSON (ErrorStack es) = object [
    "stack" .= es]
