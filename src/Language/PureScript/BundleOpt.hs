-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.BundleOpt
-- Copyright   :  (c) Jürgen Nicklisch-Franken 2016
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <juregen.nicklisch@symbolian.net>
-- Stability   :  experimental
-- Portability :
--
-- | Optimize steps added to the bundle process for purescript.
--
-- This module takes as input the Javascript AST and applies optimizations like
-- uncurrying and inlining
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.BundleOpt (
    uncurryFunc
) where

import Debug.Trace
import Data.Maybe(mapMaybe)
import Data.List (intersperse)
import Language.PureScript.BundleTypes
import Language.JavaScript.Pretty.Printer
import Language.JavaScript.Parser.AST

uncurryFunc :: [Module] -> [Module]
uncurryFunc modules =
    let modulesWithUncurried = map generateUncurried modules
    in  {-trace (show modules) $-} modulesWithUncurried

generateUncurried :: Module -> Module
generateUncurried (Module moduleIdentifier moduleElements) =
        let newEles = mapMaybe generateUncurriedEle moduleElements
        in (Module moduleIdentifier (moduleElements ++ newEles))

-- Generate uncurried functions from curried functions



generateUncurriedEle :: ModuleElement -> Maybe ModuleElement
generateUncurriedEle m@(Member jSNode sort name [decl] keys) -- a var decl
    | JSFunctionExpression fn _names lb [parameter] rb block <- node decl
    , JSLiteral "function" <- node fn
    , JSIdentifier idi <- node parameter
        = trace ("candidate: " ++ name ++ " para: " ++ show idi) $
            case analyzeUncurriedPrim [idi] block of
                Nothing -> Nothing
                Just (argList,block) -> if not sort
                                            then trace ("generateFor: " ++ name) $ generateUncurried1 argList block m
                                            else trace ("generateFor2: " ++ name) $ generateUncurried2 argList block m
  where
    generateUncurried1 :: [String] -> JSNode -> ModuleElement -> Maybe ModuleElement
    generateUncurried1 argList block (Member jsNode typ name [decl] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl
        , JSVariables var [ varIntro ] rb2 <- node jsNode
        , JSVarDecl declN (eq : decl) <- node varIntro
        = let newName = name ++ "_"
              newArgList    = intersperse (nt (JSLiteral ","))
                                    $ map (nt . JSIdentifier)
                                        argList
              newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
              newVarIntro   = NN $ JSVarDecl (sp $ JSIdentifier newName) (eq : [newDecl])
              newNode       = NN $ JSVariables var [newVarIntro] rb2
          in Just (Member newNode typ newName [newDecl] keys)
    generateUncurried _ _ _ = Nothing

    generateUncurried2 :: [String] -> JSNode -> ModuleElement -> Maybe ModuleElement
    generateUncurried2 argList block (Member jsNode typ name1 [decl] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl
        , JSExpression (e : op : decl) <- node jsNode
        =   let newName = name ++ "_"
            in case setAccessor (node e) newName of
                Nothing -> Nothing
                Just newE ->
                    let newArgList    = intersperse (nt (JSLiteral ","))
                                            $ map (nt . JSIdentifier)
                                                argList
                        newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
                        newNode       = NN $ JSExpression (NN newE : op : [newDecl])
                    in Just (Member newNode typ newName [newDecl] keys)
    generateUncurried2 _ _ _ = Nothing
generateUncurriedEle _ = Nothing

analyzeUncurriedPrim :: [String] -> JSNode -> Maybe ([String], JSNode)
analyzeUncurriedPrim idList decl
    | JSBlock _ [ef] _      <- node decl
    , JSReturn _ [ef2] _    <- node ef
    , JSExpression [ef3]    <- node ef2
    , JSFunctionExpression fn _names lb [parameter] rb block <- node ef3
    , JSLiteral "function"  <- node fn
    , JSIdentifier idi      <- node parameter
    = trace ("found deeper: " ++ show idList ++ " para: " ++ show idi) $ analyzeUncurriedPrim (idi : idList) block
analyzeUncurriedPrim l@(a:b:_) block = Just (reverse l,block)
analyzeUncurriedPrim _ _ = Nothing
