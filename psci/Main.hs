-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forever)
import Control.Monad.Trans.Class

import Data.Generics
import Data.List (isPrefixOf)

import System.Console.Haskeline

import qualified Language.PureScript as P
import qualified Control.Monad.State.Strict as S
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U (readFile)

getPreludeFilename :: IO FilePath
getPreludeFilename = Paths.getDataFileName "libraries/prelude/prelude.purs"

options :: P.Options
options = P.Options True False True False

data InterpreterState = InterpreterState { interpEnv :: [P.DoNotationElement] } deriving (Show)

defaultState :: InterpreterState
defaultState = InterpreterState []

completion :: (S.MonadState InterpreterState m) => CompletionFunc m
completion = completeWord Nothing " \t\n\r" findCompletions
  where
  findCompletions :: (S.MonadState InterpreterState m) => String -> m [Completion]
  findCompletions str = do
    st <- S.get
    let names = map show . concatMap toNames . interpEnv $ st
    let matches = filter (isPrefixOf str) names
    return $ map simpleCompletion matches
  toNames :: P.DoNotationElement -> [P.Ident]
  toNames (P.DoNotationLet binder _) = binderToNames binder
  toNames (P.DoNotationBind binder _) = binderToNames binder
  toNames _ = []
  binderToNames :: P.Binder -> [P.Ident]
  binderToNames = everything (++) (mkQ [] find)
    where
    find (P.VarBinder ident) = [ident]
    find _ = []

createTemporaryModule :: [P.DoNotationElement] -> P.Module
createTemporaryModule decls =
  let
    moduleName = P.ProperName "Temp"
    importDecl m = P.ImportDeclaration m Nothing
    prelude = P.ModuleName (P.ProperName "Prelude")
    effModule = P.ModuleName (P.ProperName "Eff")
    effMonad = P.Var (P.Qualified (Just effModule) (P.Ident "eff"))
    itDecl = P.ValueDeclaration (P.Ident "it") [] Nothing (P.Do effMonad decls)
  in
    P.Module moduleName [importDecl prelude, itDecl]

main :: IO ()
main = do
  preludeFilename <- getPreludeFilename
  preludeText <- U.readFile preludeFilename
  let (Right prelude) = P.runIndentParser P.parseModules preludeText
  flip S.evalStateT defaultState . runInputT (setComplete completion defaultSettings) $ do
    outputStrLn "PureScript Compiler (Interactive Mode)"
    forever $ do
      line <- getInputLine "> "
      case line of
        Nothing -> return ()
        Just line' ->
          case P.runIndentParser P.parseDoNotationElement line' of
            Left err -> outputStrLn (show err)
            Right decl -> do
              st <- lift S.get
              let m = createTemporaryModule (interpEnv st ++ [decl])
              case P.compile options (prelude ++ [m]) of
                Left err -> outputStrLn err
                Right (js, _, _) -> do
                  outputStrLn js
                  lift . S.put $ st { interpEnv = interpEnv st ++ [decl] }

