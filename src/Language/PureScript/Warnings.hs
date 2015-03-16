-----------------------------------------------------------------------------
--
-- Module : Language.PureScript.Warnings
-- Copyright : (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License : MIT
--
-- Maintainer : Phil Freeman <paf31@cantab.net>
-- Stability : experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.PureScript.Warnings where

import Data.Either (lefts, rights)
import Data.List (intercalate)
import Data.Monoid
import Data.Foldable (fold, foldMap)

import Control.Monad.Except
import Control.Monad.Unify
import Control.Applicative ((<$>))

import Language.PureScript.AST
import Language.PureScript.Pretty
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries

import qualified Text.PrettyPrint.Boxes as Box

-- |
-- A type of warning messages
--
data WarningMessage
 = UnusedTypeVariable ProperName
 | ShadowedTypeVariable ProperName
 | PositionedWarning SourceSpan WarningMessage
 deriving (Show)

-- |
-- Get the warning code for a particular warning type
--
warningCode :: WarningMessage -> String
warningCode (UnusedTypeVariable _)   = "UnusedTypeVariable"
warningCode (ShadowedTypeVariable _) = "ShadowedTypeVariable"
warningCode (PositionedWarning _ w)  = warningCode w

-- |
-- A stack trace for a warning
--
newtype MultipleWarnings = MultipleWarnings
 { runMultipleWarnings :: [WarningMessage] } deriving (Show, Monoid)

-- |
-- Create a warning set from a single warning message
--
warningMessage :: WarningMessage -> MultipleWarnings
warningMessage war = MultipleWarnings [war]

-- |
-- Lift a function on WarningMessage to a function on MultipleWarnings
--
onWarningMessages :: (WarningMessage -> WarningMessage) -> MultipleWarnings -> MultipleWarnings
onWarningMessages f = MultipleWarnings . map f . runMultipleWarnings
