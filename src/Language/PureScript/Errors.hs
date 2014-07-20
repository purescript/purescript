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

{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Errors
	(stringifyErrorStack,
	 mkErrorStack,
	 positionError,
	 rethrow,
	 rethrowWithPosition,
	 module Language.PureScript.DevTools.ErrorTypes)
where

import Data.Monoid

import Control.Monad.Error

import Language.PureScript.DevTools.PrettyPrint
import Language.PureScript.DevTools.JSONPrint
import Language.PureScript.DevTools.Renderers
import Language.PureScript.DevTools.ErrorTypes
import Language.PureScript.Declarations


stringifyErrorStack :: Renderer -> Bool -> Either ErrorStack a -> Either String a
stringifyErrorStack HumanReadable printFullStack = either (Left . prettyPrintErrorStack printFullStack) Right
stringifyErrorStack JSON printFullStack = either (Left . jsonPrintErrorStack printFullStack) Right

mkErrorStack :: String -> Maybe ErrorSource -> ErrorStack
mkErrorStack msg t = ErrorStack [CompileError msg t Nothing]

positionError :: SourcePos -> ErrorStack
positionError pos = ErrorStack [CompileError "" Nothing (Just pos)]

-- |
-- Rethrow an error with a more detailed error message in the case of failure
--
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

-- |
-- Rethrow an error with source position information
--
rethrowWithPosition :: (MonadError ErrorStack m) => SourcePos -> m a -> m a
rethrowWithPosition pos = rethrow (positionError pos <>)
