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

-- |
-- Pretty print a single warning, simplifying if necessary
--
prettyPrintSingleWarning :: Bool -> WarningMessage -> Box.Box
prettyPrintSingleWarning full e = prettyPrintWarningMessage (if full then e else simplifyWarningMessage e)
  where
  -- |
  -- Pretty print a WarningMessage
  --
  prettyPrintWarningMessage :: WarningMessage -> Box.Box
  prettyPrintWarningMessage em =
    paras
      [ go em
      , line ("See " ++ wikiUri ++ " for more information, or to contribute content related to this warning.") 
      ]
    where
    wikiUri :: String
    wikiUri = "https://github.com/purescript/purescript/wiki/Warning-Code-" ++ warningCode e
      
    go :: WarningMessage -> Box.Box
    go (UnusedTypeVariable name)       = line $ "Unused type variable " ++ show name
    go (ShadowedTypeVariable name)     = line $ "Shadowed type variable " ++ show name
    go (PositionedWarning pos war)     = paras [ line $ "Warning at " ++ show pos ++ ":"
                                               , indent $ go war
                                               ]

  line :: String -> Box.Box
  line = Box.text

  paras :: [Box.Box] -> Box.Box
  paras = Box.vcat Box.left

  indent :: Box.Box -> Box.Box
  indent = Box.moveRight 2

  -- |
  -- Render a DictionaryValue fit for human consumption in error messages
  --
  prettyPrintDictionaryValue :: DictionaryValue -> Box.Box
  prettyPrintDictionaryValue (LocalDictionaryValue _)           = line "Dictionary in scope"
  prettyPrintDictionaryValue (GlobalDictionaryValue nm)         = line (show nm)
  prettyPrintDictionaryValue (DependentDictionaryValue nm args) = paras [ line $ (show nm) ++ " via"
                                                                        , indent $ paras $ map prettyPrintDictionaryValue args
                                                                        ]
  prettyPrintDictionaryValue (SubclassDictionaryValue sup nm _) = paras [ line $ (show nm) ++ " via superclass"
                                                                        , indent $ prettyPrintDictionaryValue sup
                                                                        ]
  
  -- |
  -- Pretty print and export declaration
  --  
  prettyPrintExport :: DeclarationRef -> String
  prettyPrintExport (TypeRef pn _) = show pn
  prettyPrintExport (ValueRef ident) = show ident
  prettyPrintExport (TypeClassRef pn) = show pn
  prettyPrintExport (TypeInstanceRef ident) = show ident
  prettyPrintExport (PositionedDeclarationRef _ _ ref) = prettyPrintExport ref

  -- |
  -- Simplify a warning message
  --
  simplifyWarningMessage :: WarningMessage -> WarningMessage
  simplifyWarningMessage = unwrap Nothing
    where
    unwrap :: Maybe SourceSpan -> WarningMessage -> WarningMessage
    unwrap _   (PositionedWarning pos err) = unwrap (Just pos) err
    unwrap pos other = wrap pos other
  
    wrap :: Maybe SourceSpan -> WarningMessage -> WarningMessage
    wrap Nothing    = id
    wrap (Just pos) = PositionedWarning pos

-- |
-- Pretty print multiple warnings
--
prettyPrintMultipleWarnings :: Bool -> MultipleWarnings -> String
prettyPrintMultipleWarnings full  (MultipleWarnings [e]) = renderBox $
  prettyPrintSingleWarning full e 
prettyPrintMultipleWarnings full  (MultipleWarnings es) = renderBox $
  Box.vcat Box.left [ Box.text "Multiple warnings:"
                    , Box.vsep 1 Box.left $ map (Box.moveRight 2 . prettyPrintSingleWarning full) es
                    ]

renderBox :: Box.Box -> String
renderBox = unlines . map trimEnd . lines . Box.render
  where
  trimEnd = reverse . dropWhile (== ' ') . reverse
